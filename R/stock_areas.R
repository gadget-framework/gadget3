g3s_livesonareas <- function(inner_stock, areas) {
    # If no names given, add some
    if (is.null(names(areas))) {
        names(areas) <- paste0('area', areas)
    }
    stock__areas <- as.array(structure(
        as.integer(areas),
        names = names(areas)))  # NB: Force stock__areas to be an array
    stock__totalareas <- length(stock__areas)

    if (stock__totalareas == 1) {
        # Stock only in one area, so simplify
        stock__area <- as.integer(areas[[1]])
        stock__area_idx <- ~g3_idx(1)
        structure(list(
            dim = c(inner_stock$dim,
                area = stock__totalareas),
            dimnames = c(inner_stock$dimnames, list(
                area = names(stock__areas))),
            iter_ss = c(inner_stock$iter_ss, as.symbol("stock__area_idx")),
            iterate = f_substitute(~g3_with(area := stock__area, extension_point), list(
                extension_point = inner_stock$iterate), copy_all_env = TRUE),
            intersect = f_substitute(~if (area == stock__area) {
                extension_point
            }, list(extension_point = inner_stock$intersect), copy_all_env = TRUE),
            interact = f_substitute(~if (area == stock__area) g3_with(interactvar_area := area, {
                extension_point
            }), list(extension_point = inner_stock$interact), copy_all_env = TRUE),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
            name = inner_stock$name), class = c("g3_stock", "list"))
    } else {
        structure(list(
            dim = c(inner_stock$dim,
                area = stock__totalareas),
            dimnames = c(inner_stock$dimnames, list(
                area = names(stock__areas))),
            iter_ss = c(inner_stock$iter_ss, as.symbol("stock__area_idx")),
            iterate = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) g3_with(
                area := stock__areas[[stock__area_idx]],
                extension_point), list(
                    extension_point = inner_stock$iterate), copy_all_env = TRUE),
            intersect = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
                if (stock__areas[[stock__area_idx]] == area) {
                    extension_point
                    break
                }
            }, list(extension_point = inner_stock$intersect), copy_all_env = TRUE),
            interact = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
                if (stock__areas[[stock__area_idx]] == area) g3_with(interactvar_area := area, {
                    extension_point
                    break
                })
            }, list(extension_point = inner_stock$interact), copy_all_env = TRUE),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
            name = inner_stock$name), class = c("g3_stock", "list"))
    }
}

# - areagroups: list(1:2, 3)
g3s_areagroup <- function(inner_stock, areagroups) {
    # If no names given, make some up
    if (is.null(names(areagroups))) {
        names(areagroups) <- paste0('area', vapply(areagroups, function (ag) ag[[1]], numeric(1)))
    }

    stock__areagroup_lookup <- g3_intlookup(
        inner_stock$name,
        keys = as.integer(unlist(areagroups)),
        values = unlist(lapply(seq_along(areagroups),
        function (i) rep(i, times = length(areagroups[[i]])))))
    stock__minareas <- as.array(vapply(areagroups, function (x) as.integer(x[[1]]), integer(1)))

    structure(list(
        dim = c(inner_stock$dim,
            area = length(areagroups)),
        dimnames = c(inner_stock$dimnames, list(
            area = names(areagroups))),
        iterate = f_substitute(~for (stock__areagroup_idx in seq_along(stock__minareas)) g3_with(
            area := stock__minareas[[stock__areagroup_idx]], extension_point), list(
            extension_point = inner_stock$iterate), copy_all_env = TRUE),
        iter_ss = c(inner_stock$iter_ss, as.symbol("stock__areagroup_idx")),
        intersect = f_substitute(~g3_with(
            stock__areagroup_idx := g3_idx(lookup),
            if (stock__areagroup_idx > g3_idx(-1L)) extension_point), list(
                lookup = stock__areagroup_lookup('getdefault', ~area, -1L),
                extension_point = inner_stock$intersect), copy_all_env = TRUE),
        interact = f_substitute(~g3_with(
            stock__areagroup_idx := g3_idx(lookup),
            if (stock__areagroup_idx > g3_idx(-1L)) g3_with(interactvar_area := area, extension_point)), list(
                lookup = stock__areagroup_lookup('getdefault', ~area, -1L),
                extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        name = inner_stock$name), class = c("g3_stock", "list"))
}
