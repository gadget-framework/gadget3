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
        iterate = c(inner_stock$iterate, with_names(var_base_name, list(substitute(
            for (idx_var_name in seq(g3_idx(1L), g3_idx(dim_size), by = 1L)) extension_point
        , list(
            idx_var_name = as.symbol(idx_var_name),
            dim_size = length(dimnames)))))),
        iter_ss = c(inner_stock$iter_ss, with_names(var_base_name, list(as.symbol(idx_var_name)))),
        intersect = c(inner_stock$intersect, with_names(var_base_name, list(substitute(
            g3_with(idx_var_name := intersect_idx_code, extension_point),
            list(
                idx_var_name = as.symbol(idx_var_name),
                intersect_idx_code = rlang::f_rhs(intersect_idx_f)))))),
        interact = f_substitute(~for (idx_var_name in seq(g3_idx(1L), g3_idx(dim_size), by = 1L)) extension_point, list(
            idx_var_name = as.symbol(idx_var_name),
            dim_size = length(dimnames),
            extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        env = as.environment(c(as.list(inner_stock$env), as.list(environment(intersect_idx_f)))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}
