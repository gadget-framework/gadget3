g3s_stockproduct <- function(..., ignore_dims = c()) {
    stocks <- list(...)
    prefixes <- names(stocks)
    stopifnot(length(stocks) == 2)

    add_prefix <- function (l, prefix) {
        if (length(l) > 0 && nzchar(prefix)) names(l) <- paste(prefix, names(l), sep = "_")
        for (n in ignore_dims) l[[n]] <- NULL
        return(l)
    }

    structure(list(
        dim = c(
            add_prefix(stocks[[1]]$dim, prefixes[[1]]),
            add_prefix(stocks[[2]]$dim, prefixes[[2]]),
            NULL),
        dimnames = c(
            add_prefix(stocks[[1]]$dimnames, prefixes[[1]]),
            add_prefix(stocks[[2]]$dimnames, prefixes[[2]]),
            NULL),
        iter_ss = c(
            # NB: Apply renames to stock now, ideally this would become redundant
            add_prefix(lapply(
                stocks[[1]]$iter_ss,
                function (x) as.symbol(gsub("^stock__", paste0(stocks[[1]]$name, "__"), x))), prefixes[[1]]),
            add_prefix(lapply(
                stocks[[2]]$iter_ss,
                function (x) as.symbol(gsub("^stock__", paste0(stocks[[2]]$name, "__"), x))), prefixes[[2]]),
            NULL),
        iterate = quote( stop("Not implemented: intersect/interact on source stocks") ),
        intersect = quote( stop("Not implemented: intersect/interact on source stocks") ),
        interact = quote( stop("Not implemented: intersect/interact on source stocks") ),
        with = list(),
        # NB: We need at least stock__minlen && stock__upperlen for stock_reshape() to work
        #     This will need more thought if we ever do early stock__ renaming.
        env = as.environment(as.list(stocks[[1]]$env)[grepl('len$', names(stocks[[1]]$env))]),
        name_parts = c(
            stocks[[1]]$name,
            stocks[[2]]$name,
            NULL),
        name = paste(
            stocks[[1]]$name,
            stocks[[2]]$name,
            # TODO: sep should be easier to tell the difference with a regex
            sep = "_")), class = c("g3_stock", "list"))
}
