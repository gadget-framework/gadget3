g3_intlookup <- function (lookup_name, keys, values) {
    inttype_fn <- function(postfix) {
        as.symbol(paste0(
            if (is.integer(values)) 'intintlookup' else 'inttypelookup',
            '_',
            postfix))
    }

    # TODO: Implement "If not there, previous item that is"? Use map ordering, iterate through until find bigger one?
    return(function (req_type, inner_f, extra_arg = NULL) {
        inttypelookup_zip <- g3_native(r = function (keys, values) {
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
        }, cpp = '[](vector<int> keys, vector<Type> values) -> std::map<int, Type> {
            std::map<int, Type> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        }')

        inttypelookup_get <- g3_native(r = function (lookup, key) {
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
        }, cpp = '[](std::map<int, Type> lookup, int key) -> Type {
            assert(lookup.count(key) > 0);
            return lookup[key];
        }')

        inttypelookup_getdefault <- g3_native(r = function (lookup, key, def) {
            if (is.environment(lookup)) {
                out <- lookup[[as.character(key)]]
            } else {
                # NB: [1][[1]] returns NULL if key is missing (select first item, i.e. NULL, then de-list it)
                out <- if (key < 1) NULL else lookup[key][[1]]
            }
            return(if (is.null(out)) def else out)
        }, cpp = '[](std::map<int, Type> lookup, int key, Type def) -> Type {
            return lookup.count(key) > 0 ? lookup[key] : def;
        }')

        # Make intint versions of all lookup functions
        for (n in ls(environment())) {
            if (startsWith(n, "inttypelookup")) {
                fn <- get(n)
                # Strip closure, as otherwise we can't compare instances for equality
                environment(fn) <- emptyenv()
                assign(n, fn)
                attr(fn, 'g3_native_cpp') <- gsub('Type', 'int', attr(fn, 'g3_native_cpp'), fixed = TRUE)
                assign(gsub("inttypelookup", "intintlookup", n, fixed = TRUE), fn)
            }
        }

        # TODO: Make a 1-item optimisation, then the as.array() stops being necessary
        lookup <- f_substitute(g3_formula(quote( intlookup_zip(l__keys, l__values) )), list(
            intlookup_zip = inttype_fn('zip'),
            l__keys = as.symbol(paste0(lookup_name, '__keys')),
            l__values = as.symbol(paste0(lookup_name, '__values'))))
        assign(as.character(inttype_fn('zip')), get(inttype_fn('zip')), envir = environment(lookup))
        assign(paste0(lookup_name, '__keys'), as.array(as.integer(keys)), envir = environment(lookup))
        assign(paste0(lookup_name, '__values'), as.array(values), envir = environment(lookup))

        # Lookup should be defined outside the main model loop
        lookup <- g3_global_formula(init_val = lookup)

        if (!is.null(extra_arg)) {
            rv <- f_substitute(g3_formula(quote( fn(l, inner_f, extra_arg) )), list(
                fn = inttype_fn(req_type),
                l = as.symbol(paste0(lookup_name, '__lookup')),
                inner_f = inner_f,
                extra_arg = extra_arg))
        } else {
            rv <- f_substitute(g3_formula(quote( fn(l, inner_f) )), list(
                fn = inttype_fn(req_type),
                l = as.symbol(paste0(lookup_name, '__lookup')),
                inner_f = inner_f))
        }
        assign(as.character(inttype_fn(req_type)), get(inttype_fn(req_type)), envir = environment(rv))
        assign(paste0(lookup_name, '__lookup'), lookup, envir = environment(rv))

        return(rv)
    })
}

# Turn a year/step/[area]/value data.frame into a formula
g3_timeareadata <- function(lookup_name, df, value_field = 'total_weight') {
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

    # TODO: Should accept area_group
    for (n in c('year', value_field)) {
        if (is.null(df[[n]])) stop("No ", n, " field in g3_timeareadata data.frame")
    }

    # All lengths the same, no point adding to the lookup
    times <- g3s_time_convert(df$year, df$step)  # NB: if step column missing, this will be NULL
    year_mult <- as.integer(g3s_time_multiplier(times))

    # Count potential areas, 0, 1, many
    area_count <- if (is.null(df$area)) 0 else if (length(df$area) > 1 && any(df$area[[1]] != df$area)) 2 else 1
    area_mult <- if (area_count > 1) next_mult(max(times)) else 0L

    lookup <- g3_intlookup(lookup_name,
        keys = as.integer(times + (if (area_count > 1) area_mult * df$area else 0)),
        values = df[[value_field]])

    # Return formula that does the lookup
    out_f <- lookup('getdefault', remove_multzero(f_substitute(
        g3_formula(quote( area * area_mult + cur_year * year_mult + cur_step * step_mult )),
        list(
            area_mult = area_mult,
            # Mult is zero ==> There is no step.
            step_mult = if (year_mult > 1L) 1L else 0L,
            year_mult = year_mult))), 0)

    if (area_count == 1) {
        # Wrap lookup with check that we're in the correct area
        out_f <- f_substitute(
            g3_formula(quote( if (area != our_area) 0 else out_f )),
            list(our_area = df$area[[1]], out_f = out_f))
    }

    return(out_f)
}
