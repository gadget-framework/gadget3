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
        }, cpp = 'template<typename T> std::map<int, T> __fn__(array<int> keys, array<T> values) {
            std::map<int, T> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        }')
        environment(intlookup_zip) <- baseenv()

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
        environment(intlookup_get) <- baseenv()

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
        environment(intlookup_getdefault) <- baseenv()

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

    for (n in c(value_field)) {
        if (is.null(df[[n]])) stop("No ", n, " field in g3_timeareadata data.frame")
    }

    # If have a non-identity area map, apply it to data first
    if (!(is.null(areas) || identical(names(areas), as.character(areas)))) {
        storage.mode(areas) <- "integer"  # Integer-ize without losing names
        df$area <- as.integer(areas[df$area])
    }

    # Single area --> move condition outside table
    if (length(df$area) > 0 && length(unique(df$area)) == 1) {
        our_area <- df$area[[1]]
        df$area <- NULL
    } else {
        our_area <- NULL
    }

    # Generate list of keys & corresponding code for all available columns
    mult <- 1L
    combined_keys <- rep(0L, nrow(df))
    combined_code <- 0L
    for (col_name in c('step', 'year', 'area', 'age')) {
        if (!(col_name %in% names(df))) next

        if (col_name == 'year') {
            col_sym <- as.symbol('cur_year')
            col_max <- 1999
        } else if (col_name == 'step') {
            col_sym <- as.symbol('cur_step')
            col_max <- 12
        } else {
            col_sym <- as.symbol(col_name)
            # Assume maximum value is within a power of 10 of the maximum value.
            # NB: This is a bit of a shaky assumption e.g. if age column contains 1..5 but "age" can go up to 15.
            # The chance of this being a practical concern are small, especially as age is considered last.
            col_max <- max(df[[col_name]])
        }

        combined_keys <- combined_keys + df[[col_name]] * mult
        combined_code <- substitute(var * mult + combined_code, list(
            var = col_sym,
            mult = mult,
            combined_code = combined_code))
        mult <- next_mult(col_max)
    }

    lookup <- g3_intlookup(lookup_name,
        keys = as.integer(combined_keys),
        values = df[[value_field]])

    # Return formula that does the lookup
    out_f <- lookup('getdefault', f_optimize(combined_code), 0)

    if (!is.null(our_area)) {
        # Wrap lookup with check that we're in the correct area
        out_f <- f_substitute(
            quote( if (area != our_area) 0 else out_f ),
            list(our_area = our_area, out_f = out_f))
    }

    return(out_f)
}
