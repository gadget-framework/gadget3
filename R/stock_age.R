g3s_age <- function(inner_stock, minage, maxage) {
    # If these are literals, they should be integers
    stock__minage <- if(is.numeric(minage)) as.integer(minage) else minage
    stock__maxage <- if(is.numeric(minage)) as.integer(maxage) else minage

    if ('age' %in% names(inner_stock$dim)) {
        # Modify existing age dimension with new limits
        s <- inner_stock
        s$dim[['age']] <- stock__maxage - stock__minage + 1
        s$dimnames[['age']] <- paste0('age', seq(stock__minage, stock__maxage, by = 1))

        newenv <- rlang::env_clone(environment(s$iterate))
        newenv$stock__minage <- stock__minage
        newenv$stock__maxage <- stock__maxage
        environment(s$iterate) <- newenv
        environment(s$intersect) <- newenv
        environment(s$rename) <- newenv
        return(s)
    }

    structure(list(
        dim = c(inner_stock$dim, 
            age = stock__maxage - stock__minage + 1),
        dimnames = c(inner_stock$dimnames, list(
            age = paste0('age', seq(stock__minage, stock__maxage, by = 1)))),
        iterate = f_substitute(~for (age in seq(stock__minage, stock__maxage, by = 1)) g3_with(
            stock__age_idx := g3_idx(age - stock__minage + 1L), extension_point), list(
                extension_point = inner_stock$iterate), copy_all_env = TRUE),
        iter_ss = c(inner_stock$iter_ss, as.symbol("stock__age_idx")),
        intersect = f_substitute(~if (age >= stock__minage && age <= stock__maxage) g3_with(
            stock__age_idx := g3_idx(age - stock__minage + 1L), extension_point), list(
                extension_point = inner_stock$intersect), copy_all_env = TRUE),
        interact = f_substitute(~for (interactvar_age in seq(stock__minage, stock__maxage, by = 1)) g3_with(
            stock__age_idx := g3_idx(interactvar_age - stock__minage + 1L), extension_point), list(
                extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
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
            age := stock__minages[[stock__agegroup_idx]], extension_point), list(
            extension_point = inner_stock$iterate), copy_all_env = TRUE),
        iter_ss = c(inner_stock$iter_ss, as.symbol("stock__agegroup_idx")),
        intersect = f_substitute(~g3_with(
            stock__agegroup_idx := g3_idx(lookup),
            if (stock__agegroup_idx > g3_idx(-1L)) extension_point), list(
                lookup = stock__agegroup_lookup('getdefault', ~age, -1L),
                extension_point = inner_stock$intersect), copy_all_env = TRUE),
        interact = f_substitute(~for (stock__agegroup_idx in seq_along(stock__minages)) g3_with(
            interactvar_age := stock__minages[[stock__agegroup_idx]], extension_point), list(
            extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        name = inner_stock$name), class = c("g3_stock", "list"))
}
