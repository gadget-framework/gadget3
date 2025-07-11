# Timekeeping step, advances clock and stops when time is up
# - start_year: Year to start counting from
# - end_year: Stop at the end of this year (i.e. inclusive)
# - step_lengths: MFDB group or vector of steps in a year,
#       each element is a number of months, should sum to 12
# - final_year_steps: # of steps in final year, default all (i.e. inclusive)
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
g3a_time <- function(
        start_year,
        end_year,
        step_lengths = c(12),
        final_year_steps = quote( length(step_lengths) ),
        project_years = g3_parameterized("project_years", value = 0, optimise = FALSE),
        retro_years = g3_parameterized("retro_years", value = 0, optimise = FALSE),
        run_at = g3_action_order$initial,
        run_stop_at = g3_action_order$time) {
    if (inherits(step_lengths, "mfdb_group")) step_lengths <- vapply(step_lengths, length, numeric(1))
    if (is.numeric(step_lengths) && sum(step_lengths) != 12) stop("step_lengths should sum to 12 (i.e. represent a whole year)")
    if (is.call(step_lengths) && !is.call(final_year_steps)) stop("If step_lengths is a call/formula, final_year_steps should also be a call/formula")

    # If these are literals, they should be integers
    if (is.numeric(start_year)) start_year <- as.integer(start_year)
    if (is.numeric(end_year)) end_year <- as.integer(end_year)
    if (is.numeric(step_lengths)) step_lengths <- as_force_vector(as.integer(step_lengths))
    if (is.numeric(final_year_steps)) final_year_steps <- as.integer(final_year_steps)
    if (is.numeric(project_years)) project_years <- as.integer(project_years)
    # If a formula, make sure we use one definition
    if (is.call(start_year)) start_year <- g3_global_formula(init_val = start_year)
    if (is.call(end_year)) end_year <- g3_global_formula(init_val = end_year)
    if (is.call(step_lengths)) step_lengths <- g3_global_formula(init_val = step_lengths)
    if (is.call(project_years)) project_years <- g3_global_formula(init_val = project_years)

    have_projection_years <- !identical(project_years, 0L)
    have_retro_years <- !identical(retro_years, 0L)
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
        ~length(step_lengths) * (end_year - retro_years - start_year + project_years) + final_year_steps - 1L,
        list(
            # i.e. hard code a 0L (so it can be optimised out), otherwise include variable
            project_years = if (have_projection_years) quote(as_integer(project_years)) else 0L,
            retro_years = if (have_retro_years) quote(as_integer(retro_years)) else 0L,
            final_year_steps = final_year_steps )))
    total_years <- g3_global_formula(init_val = f_substitute(
        ~end_year - retro_years - start_year + project_years + 1L,
        list(
            # NB: retro_years / project_years are parameters, so will be Type
            retro_years = if (have_retro_years) quote (as_integer(retro_years)) else 0L,
            project_years = if (have_projection_years) quote(as_integer(project_years)) else 0L)))
    nll <- 0.0

    out <- new.env(parent = emptyenv())
    out[[step_id(run_at, "-")]] <- g3_step(f_substitute(~{
        debug_label("g3a_time: Start of time period")
        cur_time <- cur_time + 1L
        if (have_retro_years) if (cur_time == 0 && assert_msg(retro_years >= 0, "retro_years must be >= 0")) return(NaN)
        if (have_projection_years) if (cur_time == 0 && assert_msg(project_years >= 0, "project_years must be >= 0")) return(NaN)
        cur_year <- start_year + (cur_time %/% step_count)
        cur_year_projection <- (cur_year > end_year - retro_years)
        cur_step <- (cur_time %% step_count) + 1L
        cur_step_size <- step_lengths[[cur_step]] / 12.0
        cur_step_final <- cur_step == step_count
    }, list(
        retro_years = retro_years,
        have_projection_years = have_projection_years,
        have_retro_years = have_retro_years,
        end = NULL )))

    # Once any initial work has been performed, stop the model.
    out[[step_id(run_stop_at)]] <- ~{ if (cur_time > total_steps) return(nll) }

    # Make sure variables are defined, even without uneven_steps
    assign("cur_step_size", cur_step_size, envir = environment(out[[step_id(run_at)]]))
    assign("step_lengths", step_lengths, envir = environment(out[[step_id(run_at)]]))
    assign("end_year", end_year, envir = environment(out[[step_id(run_at)]]))
    assign("total_years", total_years, envir = environment(out[[step_id(run_at)]]))

    # Generate micro-model to build labels per year/step
    gen_dimnames <- g3_to_r(c(as.list(out), list("5" = g3_formula(quote({
        time <- c(time, sprintf('%d-%02d', cur_year, cur_step))
        REPORT(time)
        if (cur_time == 0 || cur_step == 1) {
            year <- c(year, sprintf('%d', cur_year))
            REPORT(year)
        }
    }), time = c(), year = c()) )))
    assign("gen_dimnames", gen_dimnames, envir = environment(out[[step_id(run_at)]]))

    return(as.list(out))
}
