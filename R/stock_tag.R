g3s_tag <- function(inner_stock, tag_ids, force_untagged = TRUE) {
    stopifnot(g3_is_stock(inner_stock))
    stopifnot(is.integer(tag_ids) && length(tag_ids) > 0)

    # If no names given, add some
    if (is.null(names(tag_ids))) names(tag_ids) <- tag_ids

    # Find / add the "untagged" tag
    if (0 %in% tag_ids) {
        stock__untagged_idx <- substitute(g3_idx(x), list(x = as.integer(which(tag_ids == 0L))))
    } else if (isTRUE(force_untagged)) {
        tag_ids <- c(untagged = 0, tag_ids)
        stock__untagged_idx <- quote( g3_idx(1L) )
    } else {
        stock__untagged_idx <- NA
    }

    stock__tag_ids <- as.array(structure(
        as.integer(tag_ids),
        names = names(tag_ids)))  # NB: Force stock__tag_ids to be an array

    structure(list(
        dim = c(inner_stock$dim,
            tag = length(stock__tag_ids)),
        dimnames = c(inner_stock$dimnames, list(
            tag = names(stock__tag_ids))),
        iterate = c(inner_stock$iterate, tag = quote(
            for (stock__tag_idx in seq_along(stock__tag_ids)) g3_with(
                tag := stock__tag_ids[[stock__tag_idx]], extension_point)
            )),
        iter_ss = c(inner_stock$iter_ss,
            tag = as.symbol("stock__tag_idx")),
        intersect = f_substitute(~for (stock__tag_idx in seq_along(stock__tag_ids)) {
            if (stock__tag_ids[[stock__tag_idx]] == tag) {
                extension_point
                break
            }
        }, list(extension_point = inner_stock$intersect), copy_all_env = TRUE),
        interact = f_substitute(~for (stock__tag_idx in seq_along(stock__tag_ids)) g3_with(
            interactvar_tag := stock__tag_ids[[stock__tag_idx]],
            extension_point), list(
                extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        env = as.environment(c(as.list(inner_stock$env), list(
            stock__untagged_idx = stock__untagged_idx,
            stock__tag_ids = stock__tag_ids))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}

# Return a forumula to do tag number --> stock__tag_idx
g3s_tag_reverse_lookup <- function (stock, tag_f) {
    stock__tag_ids <- stock_definition(stock, 'stock__tag_ids')

    lookup <- g3_intlookup(
        paste0(stock$name, '__tag_lookup'),
        keys = as.integer(stock__tag_ids),
        values = seq_along(stock__tag_ids))
    f_substitute(~g3_idx(l), list(l = lookup('getdefault', tag_f, 1L)))
}