# Manual dimension, another dimension whose idx is managed outside the iteration
# Can be used to divide array into stocks, where there isn't a "stock" variable for intersect to hook into
# - var_base_name: Base name for variables, e.g. 'stock'
# - dimnames: Length / names of dimension
# - intersect_idx_f: Formula that will calculate the current index when using stock_intersect()
g3s_manual <- function(inner_stock, var_base_name, dimnames, intersect_idx_f) {
    idx_var_name <- paste0("stock__", var_base_name, "_idx")

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(
                dim = c(
                    dim(stock_env[[var_name]]),
                    length(dimnames)),
                dimnames = c(
                    dimnames(stock_env[[var_name]]),
                    list(dimnames))))
        }
    }

    list(
        iterate = f_substitute(~for (idx_var_name in seq(g3_idx(1), g3_idx(dim_size), by = 1)) extension_point, list(
            idx_var_name = as.symbol(idx_var_name),
            dim_size = length(dimnames),
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol(idx_var_name))),
        iter_ss_names = c(inner_stock$iter_ss_names, var_base_name),
        intersect = f_substitute(~g3_with(idx_var_name, intersect_idx_f, extension_point), list(
            idx_var_name = as.symbol(idx_var_name),
            intersect_idx_f = intersect_idx_f,
            extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name)
}
