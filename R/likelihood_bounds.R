# Generates penalty action for every parameter, which checks bounds on each run
g3l_bounds_penalty_fromactions <- function(
        actions,
        weight = 1,
        run_at = g3_action_order$likelihood) {
  out <- new.env(parent = emptyenv())

  param_names <- names(attr(g3_to_r(actions), 'parameter_template'))
  for(param_name in param_names) {
      out[[step_id(run_at, 'g3l_bounds_penalty', param_name)]] <- f_substitute(g3_formula(quote({
        debug_label(lbl)
        if (cur_time == 0 && is.finite(lower_bound) && is.finite(upper_bound)) {
          nll <- nll + weight * ((logspace_add(1e6*(param - upper_bound)/(upper_bound - lower_bound), 0)
                         + logspace_add(1e6*(lower_bound - param )/(upper_bound - lower_bound), 0))^2)
        }
      })), list(
        weight = weight,
        lbl = paste0("g3l_bounds_penalty for ", param_name),
        param = substitute(g3_param_nodef(n), list(n = param_name)),
        lower_bound = substitute(g3_param_lower(n), list(n = param_name)),
        upper_bound = substitute(g3_param_upper(n), list(n = param_name)) ))
  }

  return(as.list(out))
}

# Generates penalty action for every param bounds in template, baking bounds into source
g3l_bounds_penalty <- function(
        actions_or_parameter_template,
        weight = 1,
        run_at = g3_action_order$likelihood) {
  out <- new.env(parent = emptyenv())

  if (!is.data.frame(actions_or_parameter_template)) {
      return(g3l_bounds_penalty_fromactions(
          actions_or_parameter_template,
          weight,
          run_at))
  }
  parameter_template <- actions_or_parameter_template

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
