g3s_age <- function(inner_stock, minage, maxage) {
    # If these are literals, they should be integers
    stock__minage <- if(is.numeric(minage)) as.integer(minage) else minage
    stock__maxage <- if(is.numeric(minage)) as.integer(maxage) else minage

    if ('age' %in% names(inner_stock$dim)) {
        # Modify existing age dimension with new limits
        s <- inner_stock
        s$dim[['age']] <- stock__maxage - stock__minage + 1
        s$dimnames[['age']] <- paste0('age', seq(stock__minage, stock__maxage, by = 1))

        s$env <- rlang::env_clone(s$env)
        s$env$stock__minage <- stock__minage
        s$env$stock__maxage <- stock__maxage
        return(s)
    }

    structure(list(
        dim = c(inner_stock$dim, 
            age = stock__maxage - stock__minage + 1),
        dimnames = c(inner_stock$dimnames, list(
            age = paste0('age', seq(stock__minage, stock__maxage, by = 1)))),
        iterate = c(inner_stock$iterate, age = quote(
                for (age in seq(stock__minage, stock__maxage, by = 1)) g3_with(
                    stock__age_idx := g3_idx(age - stock__minage + 1L), extension_point)
            )),
        iter_ss = c(inner_stock$iter_ss, age = as.symbol("stock__age_idx")),
        intersect = c(inner_stock$intersect, age = quote(
            if (stock_isdefined(age)) {
                if (age >= stock__minage && age <= stock__maxage) g3_with(
                    stock__age_idx := g3_idx(age - stock__minage + 1L), extension_point)
            } else {
                for (age in seq(stock__minage, stock__maxage, by = 1)) g3_with(
                    stock__age_idx := g3_idx(age - stock__minage + 1L), extension_point)
            } )),
        interact = c(inner_stock$interact, age = quote(
                for (interactvar_age in seq(stock__minage, stock__maxage, by = 1)) g3_with(
                    stock__age_idx := g3_idx(interactvar_age - stock__minage + 1L), extension_point)
            )),
        with = c(inner_stock$with, age = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env), list(
            stock__minage = stock__minage,
            stock__maxage = stock__maxage))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}

# - agegroups: list(1:3, 4:10)
g3s_agegroup <- function(inner_stock, agegroups) {
    # If no names given, make some up
    if (is.null(names(agegroups))) {
        names(agegroups) <- vapply(agegroups, function (ag) {
            if (all(diff(ag) == 1)) {
                paste(ag[[1]], tail(ag, 1), sep = ":")
            } else {
                paste(ag, collapse = ",")
            }
        }, character(1))
    }

    stock__agegroup_lookup <- g3_intlookup(
        paste0(inner_stock$name, '__agegroup'),
        keys = as.integer(unlist(agegroups)),
        values = unlist(lapply(seq_along(agegroups),
        function (i) rep(i, times = length(agegroups[[i]])))))
    stock__minages <- as_force_vector(vapply(agegroups, function (x) as.integer(x[[1]]), integer(1)))
    lookup_f <- stock__agegroup_lookup('getdefault', ~age, -1L)

    structure(list(
        dim = c(inner_stock$dim,
            age = length(agegroups)),
        dimnames = c(inner_stock$dimnames, list(
            age = names(agegroups))),
        iterate = c(inner_stock$iterate, age = quote(
            for (stock__agegroup_idx in seq_along(stock__minages)) g3_with(
                age := stock__minages[[stock__agegroup_idx]], extension_point)
        )),
        iter_ss = c(inner_stock$iter_ss, age = as.symbol("stock__agegroup_idx")),
        intersect = c(inner_stock$intersect, age = substitute(
            if (stock_isdefined(age)) {
                g3_with(
                    stock__agegroup_idx := g3_idx(lookup_code),
                    if (stock__agegroup_idx > g3_idx(-1L)) extension_point)
            } else {
                for (stock__agegroup_idx in seq_along(stock__minages)) g3_with(
                    interactvar_age := stock__minages[[stock__agegroup_idx]], extension_point)
            }, list(lookup_code = rlang::f_rhs(lookup_f)) )),
        interact = c(inner_stock$interact, age = quote(
            for (stock__agegroup_idx in seq_along(stock__minages)) g3_with(
                interactvar_age := stock__minages[[stock__agegroup_idx]], extension_point)
        )),
        with = c(inner_stock$with, age = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env), as.list(environment(lookup_f)), list(
            stock__minages = stock__minages))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}
