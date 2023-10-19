g3s_time_convert <- function (year_or_time, step = NULL) {
  year <- year_or_time
  if (any(step > 99)) {
    stop("The number of steps per year cannot exceed 99")
  }

  # Parse "1999-01" strings & extract step
  if (is.factor(year)) year <- as.character(year)
  if (any(is.character(year) & grepl("-", year, fixed = TRUE))) {
    s <- strsplit(year, "-")
    year <- vapply(s, function (x) as.integer(x[[1]]), integer(1))
    step <- vapply(s, function (x) as.integer(x[[2]]), integer(1))
  }

  if (is.null(step)) {
    as.integer(year) * 100L
    } else {
    as.integer(year) * 100L + as.integer(step)
    }
}

g3s_time_labels <- function (times) {
  ## Steps are included
  if (any(times %% 100 > 0)) {
        sprintf("%d-%02d",
            times %/% 100L,
            times %% 100L)
    } else {
    sprintf("%d", times %/% 100L)
    }
}

# Time dimension, useful for data objects
# - times: Vector of g3s_time_convert(year, step) for each year/step data applies to
g3s_time <- function(inner_stock, times, year = NULL, step = NULL) {
    # If year/step provided, populate times
    if (!is.null(year)) {
        if (is.null(step)) {
            times <- g3s_time_convert(year)
        } else {
            # Generate all combinations of year/step, turn into times
            times <- expand.grid(step = step, year = year)
            times <- g3s_time_convert(times$year, times$step)
        }
    }

    # time -> index lookup
    timelookup <- g3_intlookup(
        paste0('times_', inner_stock$name),
        as.integer(times),
        seq_along(times))

    idx_f <- timelookup('getdefault', f_substitute(
    ~cur_year * 100L + cur_step * step_required,
        list(
      step_required = as.integer(any(times %% 100 > 0)))), -1L)
    stock__max_time_idx <- f_substitute(~g3_idx(t), list(t = length(times)))

    structure(list(
        dim = c(inner_stock$dim,
            time = length(times)),
        dimnames = c(inner_stock$dimnames, list(
            time = g3s_time_labels(times))),
        # NB: iterate same as intersect, iterating over all time won't make sense in ~all cases
        iterate = c(inner_stock$iterate, time = substitute(
                g3_with(stock__time_idx := g3_idx(idx_f), if (stock__time_idx >= g3_idx(1L)) extension_point)
            , list(idx_f = rlang::f_rhs(idx_f)))),
        iter_ss = c(inner_stock$iter_ss, time = as.symbol("stock__time_idx")),
        intersect = c(inner_stock$intersect, time = substitute(
            g3_with(stock__time_idx := g3_idx(idx_f), if (stock__time_idx >= g3_idx(1L)) extension_point),
            list(idx_f = rlang::f_rhs(idx_f)))),
        interact = c(inner_stock$interact, time = substitute(
                g3_with(stock__time_idx := g3_idx(idx_f), if (stock__time_idx >= g3_idx(1L)) extension_point)
            , list(idx_f = rlang::f_rhs(idx_f)))),
        with = c(inner_stock$with, time = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env), as.list(environment(idx_f)), list(
            stock__max_time_idx = stock__max_time_idx))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}

# Add dimension for model time (i.e. every year/step)
g3s_modeltime <- function (inner_stock, by_year = FALSE) {
    # NB: Definitions are quote()d so they are defined at run-time as dynamic_dims (by g3a_time)
    if (by_year) {
        new_dims <- list(year = quote(as_integer(total_years)))
        new_dimnames <- list(year = quote(seq(start_year, start_year + total_years - 1L)))
        lookup <- list(year = quote(g3_idx(cur_year - start_year + 1L)))
    } else {
        new_dims <- list(time = quote(as_integer(total_steps + 1)))
        # NB: head() truncates the vector at the right length, so we don't need accounting for final_year_steps
        new_dimnames <- list(time = quote(head(sprintf("%d-%02d",
            rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)),
            rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
        lookup <- list(time = quote(g3_idx(cur_time+1L)))
    }
    structure(list(
        dim = c(inner_stock$dim, new_dims),
        dimnames = c(inner_stock$dimnames, new_dimnames),
        iterate = c(inner_stock$iterate, time = quote(extension_point)),
        iter_ss = c(inner_stock$iter_ss, lookup),
        intersect = c(inner_stock$intersect, time = quote(extension_point)),
        interact = c(inner_stock$interact, time = quote(extension_point)),
        with = c(inner_stock$with, time = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}
