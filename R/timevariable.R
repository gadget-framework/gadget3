g3_intlookup <- function (lookup_name, keys, values) {
    inttype_fn <- function(postfix) {
        as.symbol(paste0(
            if (is.integer(values)) 'intintlookup' else 'inttypelookup',
            '_',
            postfix))
    }

    # TODO: Implement "If not there, previous item that is"? Use map ordering, iterate through until find bigger one?
    return(function (req_type, inner_f) {
        inttypelookup_zip <- g3_native(r = function (keys, values) {
            list(keys = keys, values = values)
        }, cpp = '[](vector<int> keys, vector<Type> values) -> std::map<int, Type> {
            std::map<int, Type> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        }')

        inttypelookup_get <- g3_native(r = function (lookup, key) {
            out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
            if (length(out) < 1) {
                our_args <- as.list(sys.call())
                stop(key, " not in ", our_args[[2]])
            }
            return(out)
        }, cpp = '[](std::map<int, Type> lookup, int key) -> Type {
            assert(lookup.count(key) > 0);
            return lookup[key];
        }')

        inttypelookup_getdefault <- g3_native(r = function (lookup, key) {
            out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
            return(if (length(out) < 1) -1 else out)
        }, cpp = '[](std::map<int, Type> lookup, int key) -> Type {
            return lookup.count(key) > 0 ? lookup[key] : -1;
        }')

        # Make intint versions of all lookup functions
        for (n in ls(environment())) {
            if (startsWith(n, "inttypelookup")) {
                fn <- get(n)
                fn$cpp <- gsub('Type', 'int', fn$cpp, fixed = TRUE)
                assign(gsub("inttypelookup", "intintlookup", n, fixed = TRUE), fn)
            }
        }

        assign(paste0(lookup_name, '__keys'), as.integer(keys))
        assign(paste0(lookup_name, '__values'), values)
        assign(paste0(lookup_name, '__lookup'), f_substitute(~intlookup_zip(l__keys, l__values), list(
            intlookup_zip = inttype_fn('zip'),
            l__keys = as.symbol(paste0(lookup_name, '__keys')),
            l__values = as.symbol(paste0(lookup_name, '__values')))))

        f_substitute(~fn(l, inner_f), list(
            fn = inttype_fn(req_type),
            l = as.symbol(paste0(lookup_name, '__lookup')),
            inner_f = inner_f))
    })
}

# Turn a year/step/area/value data.frame into a formula
g3_timeareadata <- function(lookup_name, df) {
    lookup <- g3_intlookup(lookup_name,
        keys = as.integer(df$area * 1000000L + df$year * 100L + df$step),
        values = as.numeric(df$value))
        
    # Return formula that does the lookup
    return(lookup('get', ~area * 1000000L + cur_year * 100L + cur_step))
}
