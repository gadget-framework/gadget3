g3s_age <- function(inner_stock, minage, maxage) {
    # If these are literals, they should be integers
    stock__minage <- if(is.numeric(minage)) as.integer(minage) else minage
    stock__maxage <- if(is.numeric(minage)) as.integer(maxage) else minage

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(dim = c(dim(stock_env[[var_name]]), stock__maxage - stock__minage + 1)))
        }
    }

    list(
        iterate = f_substitute(~for (age in seq(stock__minage, stock__maxage)) g3_with(
            stock__age_idx, g3_idx(age - stock__minage + 1), extension_point), list(
                extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__age_idx"))),
        intersect = f_substitute(~if (age >= stock__minage && age <= stock__maxage) g3_with(
            stock__age_idx, g3_idx(age - stock__minage + 1), extension_point), list(
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name)
}

# - agegroups: list(1:3, 4:10)
g3s_agegroup <- function(inner_stock, agegroups) {
    stock__agegroup_lookup <- g3_intlookup(
        inner_stock$name,
        keys = as.integer(unlist(agegroups)),
        values = unlist(lapply(seq_along(agegroups),
        function (i) rep(i, times = length(agegroups[[i]])))))
    stock__minages <- vapply(agegroups, function (x) x[[1]], integer(1))

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(dim = c(dim(stock_env[[var_name]]), length(agegroups))))
        }
    }

    list(
        iterate = f_substitute(~for (stock__agegroup_idx in seq_along(stock__minages)) g3_with(
            age, stock__minages[[stock__agegroup_idx]], extension_point), list(
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__agegroup_idx"))),
        intersect = f_substitute(~g3_with(
            stock__agegroup_idx, g3_idx(lookup),
            if (stock__agegroup_idx > g3_idx(-1)) extension_point), list(
                lookup = stock__agegroup_lookup('getdefault', ~age),
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name)
}
