# Manual dimension, another dimension whose idx is managed outside the iteration
# Can be used to divide array into stocks, where there isn't a "stock" variable for intersect to hook into
# - var_base_name: Base name for variables, e.g. 'stock'
# - dimnames: Length / names of dimension
# - intersect_idx_f: Formula that will calculate the current index when using stock_intersect()
g3s_manual <- function(inner_stock, var_base_name, dimnames, intersect_idx_f) {
    idx_var_name <- paste0("stock__", var_base_name, "_idx")
    with_names <- function (n, i) structure(i, names = n)

    # Separate code / env of intersect_idx_f
    if (rlang::is_formula(intersect_idx_f)) {
        intersect_idx_c <- rlang::f_rhs(intersect_idx_f)
        env <- as.environment(c(as.list(inner_stock$env), as.list(environment(intersect_idx_f))))
    } else {
        intersect_idx_c <- intersect_idx_f
        env <- inner_stock$env
    }

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
            g3_with(idx_var_name := intersect_idx_code, if (idx_var_name >= g3_idx(1L)) extension_point),
            list(
                idx_var_name = as.symbol(idx_var_name),
                intersect_idx_code = intersect_idx_c ))))),
        interact = c(inner_stock$interact, with_names(var_base_name, list(substitute(
            for (idx_var_name in seq(g3_idx(1L), g3_idx(dim_size), by = 1L)) extension_point
        , list(
            idx_var_name = as.symbol(idx_var_name),
            dim_size = length(dimnames)))))),
        with = c(inner_stock$with, with_names(var_base_name, list(quote(extension_point)))),
        env = env,
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}
