g3s_livesonareas <- function(inner_stock, areas) {
    # If no names given, add some
    if (is.null(names(areas))) {
        names(areas) <- paste0('area', areas)
    }
    stock__areas <- as.array(structure(
        as.integer(areas),
        names = names(areas)))  # NB: Force stock__areas to be an array
    stock__totalareas <- length(stock__areas)

    # Add each area to our formulas, so they can be referenced elsewhere
    for (n in names(areas)) {
        # NB: For intersect / interact's sake, before they get de-formula'ed
        assign(paste0('area_', n), as.integer(areas[[n]]))
    }
    area_vars <- structure(
        as.list(as.integer(areas)),
        names = paste0('area_', names(areas)))

    if (stock__totalareas == 1) {
        # Stock only in one area, so simplify
        stock__area <- as.integer(areas[[1]])
        stock__area_idx <- g3_formula(quote( g3_idx(1) ))
        structure(list(
            dim = c(inner_stock$dim,
                area = stock__totalareas),
            dimnames = c(inner_stock$dimnames, list(
                area = names(stock__areas))),
            iter_ss = c(inner_stock$iter_ss, area = as.symbol("stock__area_idx")),
            iterate = c(inner_stock$iterate, area = quote(
                g3_with(area := stock__area, extension_point)
            )),
            intersect = c(inner_stock$intersect, area = quote(
                 if (area == stock__area) extension_point
            )),
            interact = c(inner_stock$interact, area = quote(
                if (area == stock__area) g3_with(interactvar_area := area, extension_point)
            )),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
            env = as.environment(c(as.list(inner_stock$env), area_vars, list(
                stock__areas = stock__areas,
                stock__totalareas = stock__totalareas,
                stock__area_idx = g3_formula(quote( g3_idx(1) )),
                stock__area = stock__area))),
            name_parts = inner_stock$name_parts,
            name = inner_stock$name), class = c("g3_stock", "list"))
    } else {
        structure(list(
            dim = c(inner_stock$dim,
                area = stock__totalareas),
            dimnames = c(inner_stock$dimnames, list(
                area = names(stock__areas))),
            iter_ss = c(inner_stock$iter_ss, area = as.symbol("stock__area_idx")),
            iterate = c(inner_stock$iterate, area = quote(
                for (stock__area_idx in seq_along(stock__areas)) g3_with(
                    area := stock__areas[[stock__area_idx]],
                    extension_point)
            )),
            intersect = c(inner_stock$intersect, area = quote(
                for (stock__area_idx in seq_along(stock__areas)) {
                    if (stock__areas[[stock__area_idx]] == area) {
                        extension_point
                        break
                    }
                }
            )),
            interact = c(inner_stock$interact, area = quote(
                for (stock__area_idx in seq_along(stock__areas)) {
                    if (stock__areas[[stock__area_idx]] == area) g3_with(interactvar_area := area, {
                        extension_point
                        break
                    })
                }
            )),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
            env = as.environment(c(as.list(inner_stock$env), area_vars, list(
                stock__areas = stock__areas,
                stock__totalareas = stock__totalareas))),
            name_parts = inner_stock$name_parts,
            name = inner_stock$name), class = c("g3_stock", "list"))
    }
}

# - areagroups: list(1:2, 3)
g3s_areagroup <- function(inner_stock, areagroups) {
    # If no names given, make some up
    if (is.null(names(areagroups))) {
        names(areagroups) <- paste0('area', vapply(areagroups, function (ag) ag[[1]], numeric(1)))
    }

    if (all(vapply(areagroups, length, integer(1)) == 1)) {
        # All groups are only 1 item long, this doesn't need areagroups
        return(g3s_livesonareas(inner_stock, unlist(areagroups)))
    }

    stock__areagroup_lookup <- g3_intlookup(
        inner_stock$name,
        keys = as.integer(unlist(areagroups)),
        values = unlist(lapply(seq_along(areagroups),
        function (i) rep(i, times = length(areagroups[[i]])))))
    stock__minareas <- as.array(vapply(areagroups, function (x) as.integer(x[[1]]), integer(1)))
    lookup_f <- stock__areagroup_lookup('getdefault', ~area, -1L)

    structure(list(
        dim = c(inner_stock$dim,
            area = length(areagroups)),
        dimnames = c(inner_stock$dimnames, list(
            area = names(areagroups))),
        iterate = c(inner_stock$iterate, area = quote(
            for (stock__areagroup_idx in seq_along(stock__minareas)) g3_with(
                area := stock__minareas[[stock__areagroup_idx]], extension_point)
        )),
        iter_ss = c(inner_stock$iter_ss, area = as.symbol("stock__areagroup_idx")),
        intersect = c(inner_stock$intersect, area = substitute(
            g3_with(
                stock__areagroup_idx := g3_idx(lookup_code),
                if (stock__areagroup_idx > g3_idx(-1L)) extension_point),
            list(lookup_code = rlang::f_rhs(lookup_f)))),
        interact = c(inner_stock$interact, area = substitute(
            g3_with(
                stock__areagroup_idx := g3_idx(lookup_code),
                if (stock__areagroup_idx > g3_idx(-1L)) g3_with(interactvar_area := area, extension_point)),
            list(lookup_code = rlang::f_rhs(lookup_f)))),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        env = as.environment(c(as.list(inner_stock$env), as.list(environment(lookup_f)), list(
            stock__areagroup_lookup = stock__areagroup_lookup,
            stock__minareas = stock__minareas))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}
