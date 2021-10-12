g3_fit <- function(model, params, rec.steps = 1, steps = 1){
  
  ## To-do: add checks for arguments
  
  ## Get and collate actions 
  fit_env <- new.env(parent = emptyenv())
  actions <- attr(model, 'actions')
  collated_actions <- g3_collate(actions) 
  all_actions <- f_concatenate(collated_actions, 
                               parent = fit_env, 
                               wrap_call = call('while', TRUE))
  
  ## ---------------------------------------------------------------------------
  ## Setup new report actions and update the action list
  ## ---------------------------------------------------------------------------
  
  report_actions <- g3a_report_history(actions = attr(model, 'actions'))
  
  
  ## ---------------------------------------------------------------------------
  ## Update actions and re-run model
  ## ---------------------------------------------------------------------------
  
  ## TODO - REMOVE EXISTING ACTIONS IF THEY EXIST
  # tmp_actions <- 
  #   actions %>% 
  #   purrr::keep(function(x){
  #   keep(function(x){
  #     if (length(x) == 0) return(TRUE)
  #     else{
  #       return(!grepl('g3a_report_stock', 
  #                     as.list(attr(x[[1]], '.Env'))$f[[2]][[3]]))
  #     }
  #   }) 
  
  ## Compile and run model
  new_model <- g3_to_r(c(actions, report_actions))
  model_output <- new_model(params)
  
  ##############################################################################
  ##############################################################################
  
  ## COLLATE OUTPUT:
  ## Parameters first
  ## ADD BOUNDS AND WHETHER OR NOT OPTIMISED
  params <- 
    unlist(params) %>% 
    tibble::enframe(name='switch')  
  
  ## --------------------------------------------------------------
  ## Catch distributions
  ## --------------------------------------------------------------
  
  ## To-do: add in age and length attributes from stock objects
  
  ## Using attribute names for processing
  tmp <- attributes(model_output)

  ## Merge together catch distribution observations and predictions
  dat <- 
    tmp[grep('^cdist_.+__num$', names(tmp))] %>%
    purrr::map(as.data.frame.table, stringsAsFactors = FALSE) %>%
    dplyr::bind_rows(.id = 'comp') %>%
    dplyr::mutate(data_function = gsub('(cdist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\2', .data$comp),
                  type = gsub('(cdist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\3', .data$comp),
                  fleetnames = gsub('(cdist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\4', .data$comp),
                  origin = gsub('(cdist)_([A-Za-z]+)_(.+)_([A-Za-z]+)_(model|obs)__num', '\\5', .data$comp),
                  name = gsub('(cdist)_([A-Za-z]+)_(.+)_([A-Za-z]+)_(model|obs)__num', '\\3.\\4', .data$comp),
                  length = gsub('len', '', .data$length) %>% as.numeric(),
                  area = as.numeric(.data$area)) %>%
    dplyr::select(-.data$comp) %>%
    extract_year_step() %>%
    tidyr::pivot_wider(names_from = .data$origin, values_from = .data$Freq) %>% 
    dplyr::rename(observed = .data$obs, predicted = .data$model) %>% 
    tibble::as_tibble()
  
  ## Maturity
  ## TO-DO ADD LOWER AND UPPER
  if (any('matp' %in% dat$type)){
    
    stockdist <- 
      dat %>% 
      dplyr::filter(.data$type == 'matp') %>%
      dplyr::group_by(.data$year, .data$step, .data$area, .data$length, .data$age, .data$name) %>%
      dplyr::mutate(pred.ratio = .data$predicted / sum(.data$predicted, na.rm = TRUE),
                    obs.ratio = .data$observed / sum(.data$observed, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(.data$name, .data$year, .data$step, .data$area, 
                    .data$stock, .data$length, .data$age, .data$fleetnames, 
                    .data$observed, .data$obs.ratio, .data$predicted, .data$pred.ratio)
    
  }else{
    stockdist <- NULL
  }
  
  ## Age and Length distributions
  if (any(c('ldist','aldist') %in% dat$type)){
    
    catchdist.fleets <- 
      dat %>% 
      dplyr::filter(.data$type %in% c('ldist', 'aldist')) %>% 
      dplyr::group_by(.data$year, .data$step, .data$area, .data$name) %>%
      dplyr::mutate(total.catch = sum(.data$observed, na.rm = TRUE),
                    total.pred = sum(.data$predicted, na.rm = TRUE),
                    obs = .data$observed,
                    pred = .data$predicted,
                    observed = .data$observed / sum(.data$observed, na.rm = TRUE),
                    predicted = .data$predicted / sum(.data$predicted, na.rm = TRUE),
                    residuals = ifelse(.data$observed == 0, NA, .data$observed - .data$predicted)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(.data$name, .data$year, .data$step, .data$area, 
                    .data$stock, .data$length, .data$age, .data$fleetnames, 
                    .data$obs, .data$total.catch, .data$observed,
                    .data$pred, .data$total.pred, .data$predicted, .data$residuals)
    
  }else{
    catchdist.fleets <- NULL
  }
  
  
  ## -------------------------------------------------------------------------
  
  ## Survey indices
  ## TODO add parameters
  if (any(grepl('^adist_.+__num$', names(tmp)))){
    
    sidat <- 
      tmp[grep('^adist_.+__num$',names(tmp))] %>%
      purrr::map(as.data.frame.table, stringsAsFactors = FALSE) %>%
      dplyr::bind_rows(.id = 'comp') %>%
      dplyr::mutate(index = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\2', .data$comp),
                    type = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\3', .data$comp),
                    fleet = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\4', .data$comp),
                    origin = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\5', .data$comp),
                    name = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\2.\\4', .data$comp),
                    length = gsub('len', '', .data$length) %>% as.numeric(),
                    area = as.numeric(.data$area)) %>%
      extract_year_step() %>%
      dplyr::select(-.data$comp) %>%
      tidyr::pivot_wider(names_from = .data$origin, values_from = .data$Freq) %>%
      dplyr::mutate(id = paste(.data$index, .data$type, .data$fleet, sep = '_')) %>% 
      dplyr::left_join(tmp[grepl('^adist_.+__params$', names(tmp))] %>% 
                         dplyr::bind_rows(.id = 'id') %>%
                         dplyr::mutate(id = gsub('^adist_|_model__params$', '', .data$id))) %>% 
      dplyr::rename(observed = .data$obs, number = .data$model, intercept = .data$alpha, slope = .data$beta) %>% 
      dplyr::mutate(predicted = ifelse(.data$type == 'log', 
                                       exp(.data$intercept) * .data$number^.data$slope,
                                       .data$intercept + .data$slope * .data$number)) %>% 
      dplyr::select(.data$name, .data$year, .data$step, .data$area, .data$length, .data$fleet, 
                    .data$index, .data$type, .data$intercept, .data$slope, 
                    .data$observed, .data$number, .data$predicted) 
    
  }else{
    sidat <- NULL
  }
  
  ## ----------------------------------------------------------------------
  #
  ## Suitability
  if (any(grepl('.history__suit_', names(tmp)))){
    
    suitability <-
      tmp[grep('.history__suit_', names(tmp))] %>% 
      purrr::map(as.data.frame.table, stringsAsFactors = F) %>%
      dplyr::bind_rows(.id = 'comp') %>%
      dplyr::mutate(stock = gsub('(.+)_(history__suit)_(.+)', '\\1', .data$comp),
                    fleet = gsub('(.+)_(history__suit)_(.+)', '\\3', .data$comp),
                    area = as.numeric(.data$area),
                    length = gsub('len','', .data$length) %>% as.numeric(),
                    age = gsub('age','', .data$age) %>% as.numeric()) %>%
      extract_year_step() %>% 
      dplyr::rename(suit = .data$Freq) %>%
      dplyr::select(.data$year, .data$step, .data$area, .data$stock, 
                    .data$length, .data$age, .data$fleet, .data$suit) %>%
      tibble::as_tibble()
    
  }else{
    suitability <- NULL
  }
  
  ## --------------------------------------------------------------------------------
  
  ## Likelihood
  if (any(grepl('^nll_', names(tmp)))){
    
    likelihood <-
      tmp[grep('^nll_', names(tmp))] %>%
      purrr::map(~tibble::tibble(time=names(.), lik_score = as.numeric(.))) %>% 
      dplyr::bind_rows(.id='lik_comp') %>% 
      dplyr::filter(!grepl('understocking', .data$lik_comp)) %>%
      dplyr::mutate(type = gsub('.+(wgt|num|weight)','\\1', .data$lik_comp),
                    component = gsub('nll_(cdist|adist)_([A-Za-z]+)_(.+)__(num|weight)', '\\3', .data$lik_comp),
                    data_type = gsub('nll_(cdist|adist)_([A-Za-z]+)_(.+)__(num|weight)', '\\1_\\2', .data$lik_comp)) %>%
      dplyr::select(-.data$lik_comp) %>%
      tidyr::pivot_wider(names_from = .data$type, values_from = .data$lik_score) %>%
      extract_year_step()  
    
  }else{
    liklihood <- NULL
  }
  
  ## ---------------------------------------------------------------------------
  ## Stock-recruitment
  ## ---------------------------------------------------------------------------
  
  ## Recruitment summed over all steps if argument rec.steps is NULL
  if (is.null(rec.steps)) rec.steps <- 1:length(get('step_lengths', envir = attr(all_actions, '.Env'))) 
  
  if (any(grepl('.history__renewalnum', names(tmp)))){
    
    stock.recruitment <-
      tmp[grepl('.history__renewalnum', names(tmp))] %>% 
      purrr::map(as.data.frame.table, stringsAsFactors = FALSE) %>% 
      dplyr::bind_rows(.id = 'comp') %>% 
      dplyr::mutate(stock = gsub('hist_(.+)__renewalnum$', '\\1', .data$comp),
                    age = gsub('age', '', .data$age) %>% as.numeric()) %>% 
      extract_year_step() %>%
      dplyr::filter(.data$age == min(.data$age),
                    .data$step %in% rec.steps) %>% ## ADD Recruit-at-age & and min(age) should be taken from stock attributes
      dplyr::group_by(.data$stock, .data$year, .data$step, .data$area, .data$age) %>% 
      dplyr::summarise(recruitment = sum(.data$Freq)) %>% 
      dplyr::ungroup() 
    
  }else{
    stock.recruitment <- NULL
  }
  
  ## ---------------------------------------------------------------------------
  ## ---------------------------------------------------------------------------
  
  ## Merge fleet and stock reports
  all_reports <- 
    tmp[grepl('.history__', names(tmp)) & !grepl('wgt|num|suit', names(tmp))] %>% 
    purrr::map(as.data.frame.table, stringsAsFactors = FALSE, responseName = 'biomass_consumed') %>% 
    dplyr::bind_rows(.id='comp') %>% 
    dplyr::mutate(stock = gsub('hist_(.+)__(.+)', '\\1', .data$comp),
                  fleet = gsub('hist_(.+)__(.+)', '\\2', .data$comp)) %>% 
    dplyr::select(-.data$comp) %>% 
    
    dplyr::left_join(
      tmp[grepl('.history__wgt', names(tmp))] %>% 
        purrr::map(as.data.frame.table, stringsAsFactors = FALSE, responseName = 'weight') %>% 
        dplyr::bind_rows(.id='comp') %>% 
        dplyr::mutate(stock = gsub('hist_(.+)__wgt', '\\1', .data$comp)) %>% 
        dplyr::select(-.data$comp) %>% 
        dplyr::left_join(
          
          tmp[grepl('.history__num', names(tmp))] %>% 
            purrr::map(as.data.frame.table, stringsAsFactors = FALSE, responseName = 'abundance') %>% 
            dplyr::bind_rows(.id='comp') %>% 
            dplyr::mutate(stock = gsub('hist_(.+)__num', '\\1', .data$comp)) %>% 
            dplyr::select(-.data$comp)
        )
    ) %>% 
    extract_year_step() %>% 
    dplyr::mutate(length = gsub('len', '', .data$length) %>% as.numeric(),
                  number_consumed = 
                    ifelse(.data$biomass_consumed == 0, 0, .data$biomass_consumed / .data$weight)) %>% 
    tibble::as_tibble()
  
  ## Stock full
  stock.full <- 
    all_reports %>% 
    dplyr::group_by(.data$stock, .data$year, .data$step, .data$area, .data$length) %>% 
    dplyr::summarise(number = sum(.data$abundance), 
                     mean_weight = mean(.data$weight)) %>% 
    dplyr::ungroup()
  
  ## Stock std
  stock.std <- 
    all_reports %>% 
    dplyr::group_by(.data$stock, .data$year, .data$step, .data$area, .data$age) %>%
    dplyr::summarise(number = sum(.data$abundance),
                     mean_length = mean(.data$length),
                     stddev_length = stats::sd(.data$length),
                     mean_weight = mean(.data$weight)) %>% 
    dplyr::ungroup()
  
  ## Stock prey
  stock.prey <- 
    all_reports %>%
    dplyr::group_by(.data$stock, .data$year, .data$step, .data$area, .data$age) %>%
    dplyr::summarise(number = sum(.data$abundance),
                     number_consumed = sum(.data$number_consumed),
                     biomass_consumed = sum(.data$biomass_consumed)) %>% 
    dplyr::mutate(mortality = -log(1 - .data$number_consumed / .data$number)/0.25) %>% 
    dplyr::ungroup()
  
  ## Predator prey
  predator.prey <- 
    all_reports %>% 
    dplyr::group_by(.data$stock, .data$fleet, .data$year, .data$step, .data$area, .data$length) %>% 
    dplyr::summarise(number_consumed = sum(.data$number_consumed),
                     biomass_consumed = sum(.data$biomass_consumed),
                     mortality = -log(1 - sum(.data$number_consumed) / sum(.data$abundance))/0.25) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$stock, .data$fleet, .data$year, .data$step, .data$area) %>% 
    dplyr::mutate(suit = .data$mortality / max(.data$mortality),
                  suit = ifelse(is.finite(.data$suit), .data$suit, 0)) %>%
    dplyr::rename(predator = .data$fleet, prey = .data$stock)
  
  ## Fleet info
  fleet.catches <- 
    predator.prey %>% 
    dplyr::group_by(.data$year, .data$area, .data$predator, .data$prey) %>% 
    dplyr::summarise(amount = sum(.data$biomass_consumed)) %>%
    dplyr::rename(fleet = .data$predator, stock = .data$prey)
  
  fleet.info <- 
    stock.full %>%
    dplyr::left_join(predator.prey %>% 
                       dplyr::select(.data$year,
                                     .data$step, 
                                     .data$area,
                                     fleet = .data$predator, 
                                     stock = .data$prey, 
                                     .data$length, 
                                     .data$suit),
                     by = c('stock', 'year', 'step', 'area', 'length')) %>%
    dplyr::group_by(.data$year, .data$step, .data$area, .data$fleet) %>%
    dplyr::summarise(harv.bio = sum(.data$suit * .data$number * .data$mean_weight)) %>%
    dplyr::left_join(fleet.catches %>% 
                       dplyr::group_by(.data$year, .data$fleet, .data$area) %>% 
                       dplyr::summarise(amount = sum(.data$amount)),
                     by = c('year', 'area', 'fleet')) %>%
    dplyr::group_by(.data$year, .data$step, .data$area, .data$fleet) %>%
    dplyr::mutate(amount = ifelse(is.na(.data$amount), 0, .data$amount),
                  harv.rate = .data$amount / .data$harv.bio)
  
  ## Suitability over fleets                 
  harv.suit <- 
    predator.prey %>%
    dplyr::group_by(.data$year, .data$step, .data$prey, .data$length) %>%
    dplyr::filter(.data$biomass_consumed > 0) %>%
    dplyr::summarise(suit = sum(.data$biomass_consumed * .data$suit) / sum(.data$biomass_consumed)) %>%
    dplyr::rename(stock = .data$prey)
  
  ## Annual F
  ## TO-DO: Add weighted mortality
  f.by.year <- 
    stock.prey %>%
    dplyr::group_by(.data$stock, .data$year, .data$area) %>%
    dplyr::summarise(catch = sum(.data$biomass_consumed),
                     num.catch = sum(.data$number_consumed),
                     F = mean(.data$mortality, na.rm = TRUE))
  
  ## Annual output
  res.by.year <- 
    stock.full %>%
    dplyr::filter(.data$step %in% steps) %>%
    dplyr::left_join(harv.suit,
                     by = c('stock', 'year', 'step', 'length')) %>%
    dplyr::group_by(.data$stock, .data$year, .data$area, .data$step) %>%
    dplyr::summarise(total.number = sum(.data$number),
                     total.biomass = sum(.data$number * .data$mean_weight),
                     harv.biomass = sum(.data$number * .data$suit * .data$mean_weight)) %>%
    dplyr::left_join(f.by.year,
                     by = c('stock', 'year', 'area')) %>% 
    dplyr::left_join(stock.recruitment %>% 
                       dplyr::group_by(.data$stock, .data$year, .data$area) %>% 
                       dplyr::summarise(recruitment = sum(.data$recruitment)),
                     by = c('stock', 'year', 'area')) %>% 
    dplyr::ungroup()
  
  out <- list(
    sidat = sidat,
    stockdist = stockdist,
    catchdist.fleets = catchdist.fleets,
    suitability = suitability,
    likelihood = likelihood,
    stock.prey = stock.prey,
    stock.std = stock.std,
    stock.full = stock.full,
    fleet.info = fleet.info,
    res.by.year = res.by.year
  )
  class(out) <- c('gadget.fit',class(out))
  return(out)
  
}

## Locates all unique stock and fleet objects from a collated actions object
find_stocks_fleets <- function(name, env){
  
  out <- list()
  obj_names <- NULL
  
  while(!identical(env, emptyenv())){
    ## Check whether the environment contains the object
    if (exists(name, envir = env, inherits = FALSE)){
      ## Check for duplicates
      if (!env[[name]]$name %in% obj_names){
        out[[env[[name]]$name]] <- env[[name]]
        obj_names <- c(obj_names, env[[name]]$name)
      }
    }  
    env <- parent.env(env)
  }
  return(out)
}

extract_year_step <- function(data){
  
  data %>% 
    tidyr::extract(col = 'time', 
                   into = c('year', 'step'), 
                   regex="^(\\d+)-(\\d+)$", convert=TRUE) %>% 
    return()
  
}

