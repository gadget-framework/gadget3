# Turn "time" dimension into separate "year" & "step", e.g. split_time(r$dstart_st_imm__num)
# NB: This doesn't copy array data, as we are only modifying the dimensions around it
ar_split_time <- function(ar) {
    dn <- dimnames(ar)
    if (!("time" %in% names(dn))) return(ar)  # Nothing to do
    split_time <- strsplit(dn$time, "-")

    time_idx <- which(names(dn) == "time")
    dn$time <- as.integer(unique(vapply(split_time, function (x) x[[2]], character(1))))
    dn <- append(dn, list(as.integer(unique(vapply(split_time, function (x) x[[1]], character(1))))), after = time_idx)
    names(dn)[[time_idx]] <- "step"
    names(dn)[[time_idx + 1]] <- "year"

    dim(ar) <- vapply(dn, length, integer(1))
    dimnames(ar) <- dn
    return(ar)
}

g3_array_agg <- function(
        ar,
        margins = NULL,
        agg = sum,
        opt_time_split = !("time" %in% margins || "time" %in% ...names()),
        opt_length_midlen = FALSE,
        ... ) {
    filter <- list(...)

    if (isTRUE(opt_time_split)) ar <- ar_split_time(ar)
    if (isTRUE(opt_length_midlen) && "length" %in% names(dim(ar))) {
        split_length <- strsplit(dimnames(ar)$length, ":")
        first_dl <- NULL
        dimnames(ar)$length <- vapply(split_length, function (x) {
            x <- as.numeric(x)
            if (is.null(first_dl) && is.infinite(x[[2]])) return(NA_real_)  # 0:Inf, e.g. otherfood
            if (is.null(first_dl)) first_dl <<- diff(x)
            if (is.infinite(x[[2]])) x[[2]] <- x[[1]] + first_dl
            mean(x)
        }, numeric(1))
    }
    if (isTRUE(opt_length_midlen) && "predator_length" %in% names(dim(ar))) {
        split_predator_length <- strsplit(dimnames(ar)$predator_length, ":")
        first_dl <- NULL
        dimnames(ar)$predator_length <- vapply(split_predator_length, function (x) {
            x <- as.numeric(x)
            if (is.null(first_dl) && is.infinite(x[[2]])) return(NA_real_)  # 0:Inf, e.g. otherfood
            if (is.null(first_dl)) first_dl <<- diff(x)
            if (is.infinite(x[[2]])) x[[2]] <- x[[1]] + first_dl
            mean(x)
        }, numeric(1))
    }

    # Prepend "age" to numeric ages
    if ("age" %in% names(filter) && is.numeric(filter$age)) filter$age <- paste0("age", filter$age)

    # Find lengthgroup that numbers sit within
    if ("length" %in% names(filter) && is.numeric(filter$length)) {
        # NB: If already calculated, don't do it again. If we do it'll be wrong (as well as a waste)
        if (!exists("split_length")) split_length <- strsplit(dimnames(ar)$length, ":")
        first_len <- vapply(split_length, function(x) as.numeric(x[[1]]), numeric(1))

        # Find index of last item that's bigger than or equal to the lower bound
        filter$length <- vapply(filter$length, function (x) max(which(x >= first_len)), integer(1))

        # Pick the appropriate dimname
        filter$length <- dimnames(ar)$length[filter$length]
    }

    # Find lengthgroup that numbers sit within
    if ("predator_length" %in% names(filter) && is.numeric(filter$predator_length)) {
        # NB: If already calculated, don't do it again. If we do it'll be wrong (as well as a waste)
        if (!exists("split_predator_length")) split_predator_length <- strsplit(dimnames(ar)$predator_length, ":")
        first_len <- vapply(split_predator_length, function(x) as.numeric(x[[1]]), numeric(1))

        # Find index of last item that's bigger than or equal to the lower bound
        filter$predator_length <- vapply(filter$predator_length, function (x) max(which(x >= first_len)), integer(1))

        # Pick the appropriate dimname
        filter$predator_length <- dimnames(ar)$predator_length[filter$predator_length]
    }

    filter <- sapply(
        names(dim(ar)),
        function (n) if (n %in% names(filter)) as.character(filter[[n]]) else quote(expr = ),
        simplify = FALSE )

    ar <- do.call("[", c(list(ar), filter, list(drop = FALSE)))

    if (!is.null(margins)) {
        # Sort by array dimensions
        margins <- names(dim(ar))[names(dim(ar)) %in% margins]
        if (length(margins) == 0) stop("No margins to aggregate by, did you set margins that don't exist in the array?")
        ar <- apply(
            ar,
            margins,
            agg,
            simplify = TRUE )
    }
    return(ar)
}
