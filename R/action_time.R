# Timekeeping step, advances clock and stops when time is up
# - start_year: Year to start counting from
# - end_year: Stop at the end of this year (i.e. inclusive)
# - steps: MFDB group or vector of steps in a year,
#       each element is a number of months, should sum to 12
# Once added to a model, the following variables will be available:
# - cur_time: Current iteration of model, starts at 0 and increments until finished
# - cur_step: Current step within individual year
# - cur_step_size: Proportion of year this step contains, e.g. quarterly = 3/12
# - cur_year: Current year
# - cur_step_final: TRUE iff this is the final step of the year
# - total_steps: Total # of iterations before model stops
g3a_time <- function(start_year, end_year, steps = as.array(c(12)), run_at = 0) {
    if ("mfdb_group" %in% class(steps)) steps <- vapply(steps, length, numeric(1))
    if (sum(steps) != 12) stop("steps should sum to 12 (i.e. represent a whole year)")

    # If these are literals, they should be integers
    if (is.numeric(start_year)) start_year <- as.integer(start_year)
    if (is.numeric(end_year)) end_year <- as.integer(end_year)

    step_lengths <- as.array(as.integer(steps))
    step_count <- ~length(step_lengths)
    cur_time <- -1L
    cur_step <- 0L
    # Initial value is first step size
    cur_step_size <- ~step_lengths[[1]] / 12.0
    cur_year <- 0L
    cur_step_final <- FALSE
    total_steps <- ~length(step_lengths) * (end_year - start_year) + length(step_lengths) - 1L

    out <- new.env(parent = emptyenv())
    out[[step_id(run_at)]] <- g3_step(f_substitute(~{
        debug_label("g3a_time")
        cur_time <- cur_time + 1L
        if (strict_mode) assert_msg(is.finite(nll), "g3a_time: nll became NaN/Inf in previous timestep")
        if (cur_time > total_steps) return(nll)
        cur_year <- start_year + (cur_time %/% step_count)
        cur_step <- (cur_time %% step_count) + 1L
        # Don't bother changing step size if it's always the same
        if (uneven_steps) cur_step_size <- step_lengths[[cur_step]] / 12.0
        cur_step_final <- cur_step == step_count
        if (trace_mode) Rprintf("** Tick: %d-%d\n", cur_year, cur_step)
    }, list(
        uneven_steps = any(diff(steps) > 0))))
    return(as.list(out))
}
