g3s_age <- function(inner_stock, minage, maxage) {
    # If these are literals, they should be integers
    stock__minage <- if(is.numeric(minage)) as.integer(minage) else minage
    stock__maxage <- if(is.numeric(minage)) as.integer(maxage) else minage

    structure(list(
        dim = c(inner_stock$dim, 
            age = stock__maxage - stock__minage + 1),
        dimnames = c(inner_stock$dimnames, list(
            age = paste0('age', seq(stock__minage, stock__maxage, by = 1)))),
        iterate = f_substitute(~for (age in seq(stock__minage, stock__maxage, by = 1)) g3_with(
            stock__age_idx, g3_idx(age - stock__minage + 1), extension_point), list(
                extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__age_idx"))),
        intersect = f_substitute(~if (age >= stock__minage && age <= stock__maxage) g3_with(
            stock__age_idx, g3_idx(age - stock__minage + 1), extension_point), list(
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name), class = c("g3_stock", "list"))
}

# - agegroups: list(1:3, 4:10)
g3s_agegroup <- function(inner_stock, agegroups) {
    # If no names given, make some up
    if (is.null(names(agegroups))) {
        names(agegroups) <- paste0('age', vapply(agegroups, function (ag) ag[[1]], numeric(1)))
    }

    stock__agegroup_lookup <- g3_intlookup(
        paste0('ages_', inner_stock$name),
        keys = as.integer(unlist(agegroups)),
        values = unlist(lapply(seq_along(agegroups),
        function (i) rep(i, times = length(agegroups[[i]])))))
    stock__minages <- as.array(vapply(agegroups, function (x) as.integer(x[[1]]), integer(1)))

    structure(list(
        dim = c(inner_stock$dim,
            age = length(agegroups)),
        dimnames = c(inner_stock$dimnames, list(
            age = names(agegroups))),
        iterate = f_substitute(~for (stock__agegroup_idx in seq_along(stock__minages)) g3_with(
            age, stock__minages[[stock__agegroup_idx]], extension_point), list(
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__agegroup_idx"))),
        intersect = f_substitute(~g3_with(
            stock__agegroup_idx, g3_idx(lookup),
            if (stock__agegroup_idx > g3_idx(-1)) extension_point), list(
                lookup = stock__agegroup_lookup('getdefault', ~age, -1),
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name), class = c("g3_stock", "list"))
}
