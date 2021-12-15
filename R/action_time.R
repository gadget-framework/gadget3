# Timekeeping step, advances clock and stops when time is up
# - start_year: Year to start counting from
# - end_year: Stop at the end of this year (i.e. inclusive)
# - steps: MFDB group or vector of steps in a year,
#       each element is a number of months, should sum to 12
# - project_years: Number of years to project after the end of the model
# Once added to a model, the following variables will be available:
# - cur_time: Current iteration of model, starts at 0 and increments until finished
# - cur_step: Current step within individual year
# - cur_step_size: Proportion of year this step contains, e.g. quarterly = 3/12
# - cur_year: Current year
# - cur_year_projection: Is this year a projection?
# - cur_step_final: TRUE iff this is the final step of the year
# - total_steps: Total # of iterations before model stops
# - total_years: Total # of years before model stops
g3a_time <- function(start_year, end_year, steps = as.array(c(12)), project_years = 0L, run_at = 0) {
    if ("mfdb_group" %in% class(steps)) steps <- vapply(steps, length, numeric(1))
    if (is.numeric(steps) && sum(steps) != 12) stop("steps should sum to 12 (i.e. represent a whole year)")
    step_lengths <- steps

    # If these are literals, they should be integers
    if (is.numeric(start_year)) start_year <- as.integer(start_year)
    if (is.numeric(end_year)) end_year <- as.integer(end_year)
    if (is.numeric(step_lengths)) step_lengths <- as.array(as.integer(step_lengths))
    if (is.numeric(project_years)) project_years <- as.integer(project_years)
    # If a formula, make sure we use one definition
    if (is.call(start_year)) start_year <- g3_global_formula(init_val = start_year)
    if (is.call(end_year)) end_year <- g3_global_formula(init_val = end_year)
    if (is.call(step_lengths)) step_lengths <- g3_global_formula(init_val = step_lengths)
    if (is.call(project_years)) project_years <- g3_global_formula(init_val = project_years)

    have_projection_years <- !identical(project_years, 0L)
    step_count <- g3_global_formula(init_val = ~length(step_lengths))
    cur_time <- -1L
    cur_step <- 0L
    # Initial value is first step size
    # NB: This is a formula since it ends up as a double by happy coicidence, so std::floor() works
    cur_step_size <- g3_global_formula(init_val = ~step_lengths[[1]] / 12.0)
    cur_year <- 0L
    cur_step_final <- FALSE
    cur_year_projection <- FALSE
    total_steps <- g3_global_formula(init_val = f_substitute(
        ~length(step_lengths) * (end_year - start_year + project_years) + length(step_lengths) - 1L,
        list(project_years = if (have_projection_years) quote(project_years) else 0L)))
    total_years <- g3_global_formula(init_val = f_substitute(
        ~end_year - start_year + project_years + 1L,
        list(project_years = if (have_projection_years) quote(project_years) else 0L)))

    out <- new.env(parent = emptyenv())
    out[[step_id(run_at)]] <- g3_step(f_substitute(~{
        debug_label("g3a_time: Start of time period")
        cur_time <- cur_time + 1L
        if (strict_mode) assert_msg(is.finite(nll), "g3a_time: nll became NaN/Inf in previous timestep")
        if (cur_time > total_steps) {
            g3_report_all()
            return(nll)
        }
        cur_year <- start_year + (cur_time %/% step_count)
        cur_year_projection <- (cur_year > end_year)
        cur_step <- (cur_time %% step_count) + 1L
        # Don't bother changing step size if it's always the same
        if (uneven_steps) cur_step_size <- step_lengths[[cur_step]] / 12.0
        cur_step_final <- cur_step == step_count
    }, list(
        uneven_steps = if(is.numeric(steps)) any(diff(steps) > 0) else TRUE)))

    # Make sure variables are defined, even without uneven_steps
    assign("cur_step_size", cur_step_size, envir = environment(out[[step_id(run_at)]]))
    assign("step_lengths", step_lengths, envir = environment(out[[step_id(run_at)]]))
    assign("end_year", end_year, envir = environment(out[[step_id(run_at)]]))
    assign("total_years", total_years, envir = environment(out[[step_id(run_at)]]))

    return(as.list(out))
}
