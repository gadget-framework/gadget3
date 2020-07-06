# Turn a year/step/area/value data.frame into a formula
g3_timeareadata <- function(lookup_name, df, missing = 0) {
    intlookup_zip <- g3_native(r = function (keys, values) {
        list(keys = keys, values = values)
    }, cpp = '[](vector<int> keys, vector<Type> values) -> std::map<int, Type> {
        std::map<int, Type> lookup = {};

        assert(keys.size() == values.size());
        for (size_t i = 0; i < keys.size(); ++i) {
            lookup[keys[i]] = values[i];
        }
        return lookup;
    }')

    # TODO: How would you implement "If not there, previous item that is"? Use map ordering, iterate through until find bigger one?
    intlookup_get <- g3_native(r = function (lookup, key) {
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

    # Define keys, values, and combining the two
    # TODO: Ideally the generation of of the key would be dynamic, based on DF columns
    assign(paste0(lookup_name, '__keys'), as.integer(df$area * 1000000L + df$year * 100L + df$step))
    assign(paste0(lookup_name, '__values'), as.numeric(df$value))  # NB: Needs to be numeric since we use Type above
    assign(paste0(lookup_name, '__lookup'), f_substitute(~intlookup_zip(l__keys, l__values), list(
        l__keys = as.symbol(paste0(lookup_name, '__keys')),
        l__values = as.symbol(paste0(lookup_name, '__values')))))
    return(f_substitute(~intlookup_get(lookup,  area * 1000000L + cur_year * 100L + cur_step), list(
        lookup = as.symbol(paste0(lookup_name, '__lookup')))))
        
}
