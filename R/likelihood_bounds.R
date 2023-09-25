# Generates penalty action for every param bounds in template
g3l_bounds_penalty <- function(
        parameter_template,
        weight = 1,
        run_at = 10) {
  out <- new.env(parent = emptyenv())

  for(i in 1:nrow(parameter_template)) {
    if (parameter_template[i, 'optimise'] && !is.na(parameter_template[i, 'lower']) && !is.na(parameter_template[i, 'upper'])) {
      out[[step_id(run_at, 'g3l_bounds_penalty', parameter_template[i, 'switch'])]] <- f_substitute(g3_formula(quote({
        debug_label(lbl)
        if (cur_time == 0) {
          nll <- nll + weight * ((logspace_add(1e6*(g3_param(param)- upper_bound)/(upper_bound-lower_bound), 0)
                         + logspace_add(1e6*(lower_bound - g3_param(param) )/(upper_bound-lower_bound), 0))^2)
        }
      })), list(
        weight = weight,
        lbl = paste0("g3l_bounds_penalty for ", parameter_template[i, 'switch']),
        param = parameter_template[i, 'switch'],
        upper_bound = parameter_template[i, 'upper'],
        lower_bound = parameter_template[i, 'lower']))
    }
  }

  return(as.list(out))
}
