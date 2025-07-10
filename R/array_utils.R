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
        agg = c(
            "sum",
            "length_mean", "length_sd",
            "predator_length_mean", "predator_length_sd" ),
        opt_time_split = !("time" %in% margins || "time" %in% ...names()),
        opt_length_midlen = FALSE,
        ... ) {
    if (is.character(agg)) {
        to_vec <- function (x, meas) {
            # If more than one dimension, collapse down to just meas
            if (is.array(x)) x <- g3_array_agg(x, meas)

            return(x)
        }

        agg <- match.arg(agg)
        if (identical(agg, "sum")) {
            agg <- base::sum
        } else if (endsWith(agg, "_mean")) {
            meas <- gsub("_mean$", "", agg)
            agg <- function (x) {
                # Values are counts, names are measurement groups
                wt <- to_vec(x, meas)
                x <- as.numeric(names(wt))
                xmean <- sum(x * wt) / sum(wt)

                return(xmean)
            }
            opt_length_midlen <- TRUE
        } else if (endsWith(agg, "_sd")) {
            meas <- gsub("_sd$", "", agg)
            agg <- function (x) {
                # Values are counts, names are measurement groups
                wt <- to_vec(x, meas)
                x <- as.numeric(names(wt))
                xmean <- sum(x * wt) / sum(wt)

                return(sqrt( sum(wt * (x - xmean)^2) / (sum(wt) - 1) ))
            }
            opt_length_midlen <- TRUE
        } else stop("Unknown agg function name '", agg, "'")
    } else {
        stopifnot(is.character(agg) || is.function(agg))
    }

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
        # If simplify goes too far, turn back to an array
        if (is.vector(ar)) {
            ar <- as.array(ar)
            # as.array() doesn't restore dimension names, so do that from margins
            dn <- structure(dimnames(ar), names = margins[[1]])
            dim(ar) <- vapply(dn, length, integer(1))
            dimnames(ar) <- dn
        }
    }
    return(ar)
}

g3_array_combine <- function(
        arrays,
        agg = sum,
        init_val = 0 ) {
    # Return something dimnames-ish, even for a vector
    gac_dimnames <- function (ar) {
        if (is.array(ar)) dimnames(ar) else list("__gac_vec" = names(ar))
    }

    # Return sorted union of arrays
    uniondn <- function(args) {
        inner <- function (dn_a, dn_b) {
            sapply(names(dn_a), function(n) {
                # Find out which of dn_a & dn_b contains the first item of the other
                if (length(aidx <- which(dn_a[[n]] == dn_b[[n]][[1]])) > 0) {
                    out <- dn_a[[n]]
                    # Splice dn_b into dn_a, letting R expand the array
                    out[aidx:(aidx + length(dn_b[[n]]) - 1)] <- dn_b[[n]]

                } else if (length(bidx <- which(dn_b[[n]] == dn_a[[n]][[1]])) > 0) {
                    out <- dn_b[[n]]
                    # Splice dn_a into dn_b, letting R expand the array
                    out[bidx:(bidx + length(dn_a[[n]]) - 1)] <- dn_a[[n]]

                } else {
                    stop("No intersection between ", paste(dn_a[[n]], collapse = ","), " and ", paste(dn_b[[n]], collapse = ","))
                }
                out
            }, simplify = FALSE)
        }

        if (length(args) == 0) stop("You must provide at least one argument")
        if (length(args) == 1) return(args[[1]])
        if (length(args) == 2) return(inner(args[[1]], args[[2]]))
        return(uniondn(c(
            list(inner(args[[1]], args[[2]])),
            tail(args, -2) )))
    }

    flatten_lists <- function (l) {
        # Base-case: Return single-item list
        if (!is.list(l)) return(list(l))

        # Recurse for each element in l, generating a list of lists
        out <- lapply(l, flatten_lists)
        # Concatenate all lists in out
        do.call(c, out)
    }

    arrays <- flatten_lists(arrays)
    for (i in seq_along(arrays)) if (i > 1) {
        # Names of dimensions should match
        stopifnot(all.equal(
            names(dim(arrays[[i]])),
            names(dim(arrays[[1]])) ))
    }

    dn <- uniondn(lapply(arrays, gac_dimnames))
    # Start with vector / array of init_val
    out <- if (identical(names(dn), "__gac_vec")) structure(rep(init_val, length(dn[[1]])), names = dn[[1]]) else array(init_val, dim = sapply(dn, length), dimnames = dn)
    for (ar in arrays) {
        if (identical(gac_dimnames(ar), gac_dimnames(out))) {
            out <- out + ar
        } else {
            # Convert ar's dimnames into a subset
            subset <- as.call(c(list(as.symbol("["), quote(out)), gac_dimnames(ar) ))
            eval(substitute(subset <- subset + ar, list(subset = subset)))
        }
    }
    # If array-ness got dropped, put it back
    if (is.vector(out) && !identical(names(dn), "__gac_vec")) {
        out <- as.array(out)
        # as.array() doesn't restore dimension names
        dn <- structure(dimnames(ar), names = names(dn))
        dim(ar) <- vapply(dn, length, integer(1))
        dimnames(ar) <- dn
    }
    return(out)
}

g3_array_plot <- function (
        ar,
        legend = "topright" ) {
    ar <- as.array(ar)
    if (is.null(dim(ar)) || length(dim(ar)) == 1) {
        graphics::plot(
            ar,
            type = "l",
            xaxt = "n",
            xlab = paste(names(dim(ar)), collapse = " / "),
            ylab = deparse1(sys.call()[[2]]),
            col = grDevices::rainbow(1) )
        graphics::axis(1, at = seq_len(nrow(ar)), labels = rownames(ar))
    } else if (length(dim(ar)) == 2) {
        colors <- grDevices::rainbow(ncol(ar))
        for (i in seq_len(ncol(ar))) {
            if (i == 1) {
                graphics::plot(
                    ar[,i],
                    type = "l",
                    xaxt = "n",
                    xlab = names(dim(ar))[[1]],
                    ylab = deparse1(sys.call()[[2]]),
                    ylim = range(ar),
                    col = colors[[i]] )
                graphics::axis(1, at = seq_len(nrow(ar)), labels = rownames(ar))
            } else {
                graphics::lines(ar[,i], col = colors[[i]])
            }
        }
        legend(legend, dimnames(ar)[[2]], lty = 1, bg = "white", col = colors, x.intersp = 1)
    } else {
        stop("Unsupported number of dimensions: ", paste(dim(ar)))
    }
}
