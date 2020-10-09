# Timekeeping step, advances clock and stops when time is up
# - start_year: Year to start counting from
# - end_year: Stop at the end of this year (i.e. inclusive)
# - steps: MFDB group or vector of steps in a year,
#       each element is a number of months, should sum to 12
# Once added to a model, the following variables will be available:
# - cur_time: Current iteration of model, starts at 0 and increments until finished
# - cur_step: Current step within individual year
# - cur_step_len: How many months long this step is
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
    step_count <- length(steps)
    cur_time <- as.integer(-1)
    cur_step <- as.integer(0)
    cur_step_len <- as.integer(0)
    cur_year <- as.integer(0)
    cur_step_final <- FALSE
    total_steps <- ~length(step_lengths) * (end_year - start_year) + length(step_lengths) - 1

    out <- list()
    out[[step_id(run_at)]] <- ~{
        comment("g3a_time")
        cur_time <- cur_time + 1
        if (cur_time > total_steps) return(nll)
        cur_year <- start_year + (cur_time %/% step_count)
        cur_step <- (cur_time %% step_count) + 1
        cur_step_len <- step_lengths[[cur_step]]
        cur_step_final <- cur_step == step_count
        Rprintf("** Tick: %d-%d\n", cur_year, cur_step)
    }
    return(out)
}
