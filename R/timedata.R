g3_intlookup <- function (lookup_name, keys, values) {
    # TODO: Implement "If not there, previous item that is"? Use map ordering, iterate through until find bigger one?
    return(function (req_type, inner_f, extra_arg = NULL) {
        intlookup_zip <- g3_native(r = function (keys, values) {
            if (min(keys) > 0 && max(keys) < 1e+05) {
                out <- list()
                out[as.integer(keys)] <- as.list(values)
            } else {
                out <- as.list(values)
                names(out) <- keys
                out <- as.environment(out)
            }
            attr(out, 'key_var') <- deparse(sys.call()[[2]])
            attr(out, 'value_var') <- deparse(sys.call()[[3]])
            return(out)
        }, cpp = 'template<typename T> std::map<int, T> __fn__(vector<int> keys, vector<T> values) {
            std::map<int, T> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        }')
        environment(intlookup_zip) <- emptyenv()

        intlookup_get <- g3_native(r = function (lookup, key) {
            if (is.environment(lookup)) {
                out <- lookup[[as.character(key)]]
            } else {
                # NB: [1][[1]] returns NULL if key is missing (select first item, i.e. NULL, then de-list it)
                out <- if (key < 1) NULL else lookup[key][[1]]
            }
            if (is.null(out)) {
                our_args <- as.list(sys.call())
                stop(key, " not in ", our_args[[2]])
            }
            return(out)
        }, cpp = 'template<typename T> T __fn__(std::map<int, T> lookup, int key) {
            assert(lookup.count(key) > 0);
            return lookup[key];
        }')
        environment(intlookup_get) <- emptyenv()

        intlookup_getdefault <- g3_native(r = function (lookup, key, def) {
            if (is.environment(lookup)) {
                out <- lookup[[as.character(key)]]
            } else {
                # NB: [1][[1]] returns NULL if key is missing (select first item, i.e. NULL, then de-list it)
                out <- if (key < 1) NULL else lookup[key][[1]]
            }
            return(if (is.null(out)) def else out)
        }, cpp = 'template<typename T, typename DefT> T __fn__(std::map<int, T> lookup, int key, DefT def) {
            return lookup.count(key) > 0 ? lookup[key] : (T)def;
        }')
        environment(intlookup_getdefault) <- emptyenv()

        # TODO: Make a 1-item optimisation, then the as.array() stops being necessary
        lookup <- f_substitute(quote( intlookup_zip(l__keys, l__values) ), list(
            l__keys = as.symbol(paste0(lookup_name, '__keys')),
            l__values = as.symbol(paste0(lookup_name, '__values'))))
        assign('intlookup_zip', intlookup_zip, , envir = environment(lookup))
        assign(paste0(lookup_name, '__keys'), as.array(as.integer(keys)), envir = environment(lookup))
        assign(paste0(lookup_name, '__values'), as.array(values), envir = environment(lookup))

        # Lookup should be defined outside the main model loop
        lookup <- g3_global_formula(init_val = lookup)

        if (!is.null(extra_arg)) {
            rv <- f_substitute(quote( fn(l, inner_f, extra_arg) ), list(
                fn = as.symbol(paste0('intlookup_', req_type)),
                l = as.symbol(paste0(lookup_name, '__lookup')),
                inner_f = inner_f,
                extra_arg = extra_arg))
        } else {
            rv <- f_substitute(quote( fn(l, inner_f) ), list(
                fn = as.symbol(paste0('intlookup_', req_type)),
                l = as.symbol(paste0(lookup_name, '__lookup')),
                inner_f = inner_f))
        }
        assign(paste0('intlookup_', req_type), get(paste0('intlookup_', req_type)), envir = environment(rv))
        assign(paste0(lookup_name, '__lookup'), lookup, envir = environment(rv))

        return(rv)
    })
}

# Turn a year/step/[area]/value data.frame into a formula
g3_timeareadata <- function(lookup_name, df, value_field = 'total_weight', areas = NULL) {
    stopifnot(is.null(areas) || !anyNA(as.integer(areas)))

    # What's the next power of 10?
    next_mult <- function (x) as.integer(10 ** ceiling(log10(x)))

    remove_multzero <- function (f) call_replace(f, "*" = function (x) {
        if (isTRUE(all.equal(x[[2]], 0))) return(0)
        if (isTRUE(all.equal(x[[3]], 0))) return(0)
        return(as.call(list(
            x[[1]],
            remove_multzero(x[[2]]),
            remove_multzero(x[[3]]))))
    })

    for (n in c('year', value_field)) {
        if (is.null(df[[n]])) stop("No ", n, " field in g3_timeareadata data.frame")
    }

    # All lengths the same, no point adding to the lookup
    times <- g3s_time_convert(df$year, df$step)  # NB: if step column missing, this will be NULL
    year_mult <- as.integer(g3s_time_multiplier(times))

    # If have a non-identity area map, apply it to data first
    if (!(is.null(areas) || identical(names(areas), as.character(areas)))) {
        storage.mode(areas) <- "integer"  # Integer-ize without losing names
        df$area <- as.integer(areas[df$area])
    }

    # Count potential areas, 0, 1, many
    area_count <- if (is.null(df$area)) 0 else if (length(df$area) > 1 && any(df$area[[1]] != df$area)) 2 else 1
    area_mult <- if (area_count > 1) next_mult(max(times)) else 0L

    lookup <- g3_intlookup(lookup_name,
        keys = as.integer(times + (if (area_count > 1) area_mult * df$area else 0)),
        values = df[[value_field]])

    # Return formula that does the lookup
    out_f <- lookup('getdefault', remove_multzero(f_substitute(
        quote( area * area_mult + cur_year * year_mult + cur_step * step_mult ),
        list(
            area_mult = area_mult,
            # Mult is zero ==> There is no step.
            step_mult = if (year_mult > 1L) 1L else 0L,
            year_mult = year_mult))), 0)

    if (area_count == 1) {
        # Wrap lookup with check that we're in the correct area
        out_f <- f_substitute(
            quote( if (area != our_area) 0 else out_f ),
            list(our_area = df$area[[1]], out_f = out_f))
    }

    return(out_f)
}