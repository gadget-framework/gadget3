g3s_dynlen <- function (inner_stock) {

    structure(list(
        dim = c(inner_stock$dim, list(
            dynlen = 1)),
        dimnames = c(inner_stock$dimnames, list(
            dynlen = "")),
        iterate = c(inner_stock$iterate, dynlen = quote(
                g3_with(
                    stock__dynlen_idx := g3_idx(1L),
                    length := stock_ss(stock__dynlen),
                    stock__midlen := stock_ss(stock__dynlen),
                    extension_point )
            )),
        iter_ss = c(inner_stock$iter_ss, dynlen = as.symbol("stock__dynlen_idx")),
        intersect = c(inner_stock$intersect, dynlen = quote(
                if (stock_isdefined(length)) {
                    # TODO: Not actually intersecting, & considering length. Is this going to be a headache?
                    g3_with(
                        stock__dynlen_idx := g3_idx(1L),
                        stock__midlen := stock_ss(stock__dynlen),
                        extension_point )
                } else {
                    g3_with(
                        stock__dynlen_idx := g3_idx(1L),
                        length := stock_ss(stock__dynlen),
                        stock__midlen := stock_ss(stock__dynlen),
                        extension_point )
                }
            )),
        interact = c(inner_stock$interact, dynlen = quote(
                g3_with(
                    stock__dynlen_idx := g3_idx(1L),
                    interactvar_length := stock_ss(stock__dynlen),
                    stock__midlen := stock_ss(stock__dynlen),
                    extension_point )
            )),
        with = c(inner_stock$with, dynlen = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env), list(
            ))),   # TODO: ?
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}

