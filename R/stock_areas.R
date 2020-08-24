g3s_livesonareas <- function(inner_stock, areas) {
    stock__areas <- as.array(as.integer(areas))  # NB: Force stock__areas to be an array
    stock__totalareas <- length(stock__areas)

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(dim = c(dim(stock_env[[var_name]]), stock__totalareas)))
        }
    }

    if (stock__totalareas == 1) {
        # Stock only in one area, so simplify
        stock__area <- as.integer(areas[[1]])
        stock__area_idx <- ~g3_idx(1)
        list(
            iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__area_idx"))),
            iter_ss_names = c(inner_stock$iter_ss_names, 'area'),
            iterate = f_substitute(~g3_with(area, stock__area, extension_point), list(
                extension_point = inner_stock$iterate)),
            intersect = f_substitute(~if (area == stock__area) {
                extension_point
            }, list(extension_point = inner_stock$intersect)),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
            name = inner_stock$name)
    } else {
        list(
            iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__area_idx"))),
            iter_ss_names = c(inner_stock$iter_ss_names, 'area'),
            iterate = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) g3_with(
                area, stock__areas[[stock__area_idx]],
                extension_point), list(
                    extension_point = inner_stock$iterate)),
            intersect = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
                if (stock__areas[[stock__area_idx]] == area) {
                    extension_point
                    break
                }
            }, list(extension_point = inner_stock$intersect)),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
            name = inner_stock$name)
    }
}

# - areagroups: list(1:2, 3)
g3s_areagroup <- function(inner_stock, areagroups) {
    stock__areagroup_lookup <- g3_intlookup(
        inner_stock$name,
        keys = as.integer(unlist(areagroups)),
        values = unlist(lapply(seq_along(areagroups),
        function (i) rep(i, times = length(areagroups[[i]])))))
    stock__minareas <- as.array(vapply(areagroups, function (x) as.integer(x[[1]]), integer(1)))

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(dim = c(dim(stock_env[[var_name]]), length(areagroups))))
        }
    }

    list(
        iterate = f_substitute(~for (stock__areagroup_idx in seq_along(stock__minareas)) g3_with(
            area, stock__minareas[[stock__areagroup_idx]], extension_point), list(
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__areagroup_idx"))),
        iter_ss_names = c(inner_stock$iter_ss_names, 'area'),
        intersect = f_substitute(~g3_with(
            stock__areagroup_idx, g3_idx(lookup),
            if (stock__areagroup_idx > g3_idx(-1)) extension_point), list(
                lookup = stock__areagroup_lookup('getdefault', ~area),
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name)
}
