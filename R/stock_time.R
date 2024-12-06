g3s_time_convert <- function (year_or_time, step = NULL) {
  year <- year_or_time
  if (any(is.numeric(step) & step > 99)) {
    stop("The number of steps per year cannot exceed 99")
  }

  # Parse "1999-01" strings & extract step
  if (is.factor(year)) year <- as.character(year)
  if (any(is.character(year) & grepl("-", year, fixed = TRUE))) {
    s <- strsplit(year, "-")
    year <- vapply(s, function (x) as.integer(x[[1]]), integer(1))
    step <- vapply(s, function (x) as.integer(x[[2]]), integer(1))
  }

  if (is.null(step) || all(step == 'all')) {
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
        paste0(inner_stock$name, '__times'),
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
        new_dimnames <- list(year = quote( attributes(gen_dimnames(param))[['year']] ))
        lookup <- list(year = quote(g3_idx(cur_year - start_year + 1L)))
    } else {
        new_dims <- list(time = quote(as_integer(total_steps + 1L)))
        # NB: head() truncates the vector at the right length, so we don't need accounting for final_year_steps
        new_dimnames <- list(time = quote( attributes(gen_dimnames(param))[['time']] ))
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

# Add dimension for fishing year, (year_length) years long, starting at (start_step)
g3s_modeltime_fishingyear <- function (inner_stock, year_length = 1, start_step = 1) {
    stopifnot(g3_is_stock(inner_stock))
    stopifnot(is.numeric(year_length))
    stopifnot(is.numeric(start_step))
    year_length <- as.integer(year_length)
    start_step <- as.integer(start_step)

    # Year that start_step sits in
    # NB: These could be stock__fishingyear_start, but for that dynamic dims would need to be stock_rename()ed
    fishing_startyr_c <- substitute(
        start_year + ((start_step - 1L) %/% step_count),
        list( year_length = year_length, start_step = start_step ))
    # Total number of steps in fishing calendar (excluding start), divided by fishing year length, rounded up
    fishing_years_c <- substitute(
        as_integer(ceiling( as.numeric(total_steps - start_step + 1L) / as.numeric(step_count * year_length) )) + 1L,
        list( year_length = year_length, start_step = start_step ))

    new_dim <- fishing_years_c
    new_dimnames <- substitute(
        paste(
            seq.int(fishing_startyr_c, length.out = fishing_years_c, by = year_length),
            seq.int(fishing_startyr_c + year_length, length.out = fishing_years_c, by = year_length),
            sep = ":" ),
        list( fishing_startyr_c = fishing_startyr_c, fishing_years_c = fishing_years_c, year_length = year_length ))
    if (start_step > 1) {
        # Add remainder year for start of model
        new_dim <- substitute(new_dim + 1L, list(new_dim = new_dim))
        new_dimnames <- substitute(c(
            list(paste(start_year, fishing_startyr_c, sep = ":")),
            new_dimnames ), list(fishing_startyr_c = fishing_startyr_c, new_dimnames = new_dimnames))
    }

    lookup <- substitute(
        g3_idx(max(
            ((cur_time - start_step + 1L) %/% (step_count * year_length)) + offset,
            1L )), list(
            offset = if (start_step > 1) 2L else 1L,
            year_length = year_length,
            start_step = start_step ))
    code <- substitute(g3_with(
        stock__fishingyear_step :=
            if (cur_time + 1L < start_step) cur_time + 1L else (cur_time - start_step + 1L) %% (step_count * year_length) + 1L,
        stock__fishingyear_revstep :=
            if (cur_time + 1L < start_step) cur_time - start_step + 1L else (cur_time - start_step + 1L) %% (step_count * year_length) - (step_count * year_length),
        stock__fishingyear_idx := lookup,
        extension_point ), list(
            lookup = lookup,
            offset = if (start_step > 1) 2L else 1L,
            year_length = year_length,
            start_step = start_step ))
    structure(list(
        dim = c(inner_stock$dim, fishingyear = new_dim),
        dimnames = c(inner_stock$dimnames, fishingyear = new_dimnames),
        iterate = c(inner_stock$iterate, fishingyear = code),
        # NB: We can't use an _idx variable currently, as we have to do stock_with(stock_ss(..)) as part of an expression, so the definitions get lost
        iter_ss = c(inner_stock$iter_ss, fishingyear = lookup),
        intersect = c(inner_stock$intersect, fishingyear = code),
        interact = c(inner_stock$interact, fishingyear = code),
        with = c(inner_stock$with, fishingyear = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env), list(
            stock__fishingyear_startyear = fishing_startyr_c,
            stock__fishingyear_stepcount = fishing_years_c ))),
        #env = as.environment(c(as.list(inner_stock$env))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}
