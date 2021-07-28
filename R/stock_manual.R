# Manual dimension, another dimension whose idx is managed outside the iteration
# Can be used to divide array into stocks, where there isn't a "stock" variable for intersect to hook into
# - var_base_name: Base name for variables, e.g. 'stock'
# - dimnames: Length / names of dimension
# - intersect_idx_f: Formula that will calculate the current index when using stock_intersect()
g3s_manual <- function(inner_stock, var_base_name, dimnames, intersect_idx_f) {
    idx_var_name <- paste0("stock__", var_base_name, "_idx")
    with_names <- function (n, i) structure(i, names = n)

    structure(list(
        dim = c(inner_stock$dim,
            with_names(var_base_name, length(dimnames))),
        dimnames = c(inner_stock$dimnames,
            with_names(var_base_name, list(dimnames))),
        iterate = f_substitute(~for (idx_var_name in seq(g3_idx(1L), g3_idx(dim_size), by = 1L)) extension_point, list(
            idx_var_name = as.symbol(idx_var_name),
            dim_size = length(dimnames),
            extension_point = inner_stock$iterate), copy_all_env = TRUE),
        iter_ss = c(inner_stock$iter_ss, as.symbol(idx_var_name)),
        intersect = f_substitute(~g3_with(idx_var_name := intersect_idx_f, extension_point), list(
            idx_var_name = as.symbol(idx_var_name),
            intersect_idx_f = intersect_idx_f,
            extension_point = inner_stock$intersect), copy_all_env = TRUE),
        interact = f_substitute(~for (idx_var_name in seq(g3_idx(1L), g3_idx(dim_size), by = 1L)) extension_point, list(
            idx_var_name = as.symbol(idx_var_name),
            dim_size = length(dimnames),
            extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        name = inner_stock$name), class = c("g3_stock", "list"))
}
