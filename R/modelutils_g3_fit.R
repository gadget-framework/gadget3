g3_fit <- function(model, params, rec.steps = 1, steps = 1){
  
  ## To-do: add checks for arguments
  
  ## Get and collate actions 
  fit_env <- new.env(parent = emptyenv())
  actions <- attr(model, 'actions')
  all_actions <- gadget3:::g3_collate(actions) %>%
    gadget3:::f_concatenate(., parent = fit_env, wrap_call = call('while', TRUE))
  
  ## ---------------------------------------------------------------------------
  ## Setup new report actions and update the action list
  ## ---------------------------------------------------------------------------
  
  report_actions <- g3a_report_history(actions = all_actions)
  
  
  ## ---------------------------------------------------------------------------
  ## Update actions and re-run model
  ## ---------------------------------------------------------------------------
  
  ## TODO - REMOVE EXISTING ACTIONS IF THEY EXIST
  # tmp_actions <- 
  #   actions %>% 
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
    dplyr::mutate(data_function = gsub('(cdist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\2', comp),
                  type = gsub('(cdist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\3', comp),
                  fleetnames = gsub('(cdist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\4', comp),
                  origin = gsub('(cdist)_([A-Za-z]+)_(.+)_([A-Za-z]+)_(model|obs)__num', '\\5', comp),
                  name = gsub('(cdist)_([A-Za-z]+)_(.+)_([A-Za-z]+)_(model|obs)__num', '\\3.\\4', comp),
                  length = gsub('len', '', length) %>% as.numeric(),
                  area = as.numeric(area)) %>%
    select(-comp) %>%
    extract_year_step() %>%
    tidyr::pivot_wider(names_from = 'origin', values_from = 'Freq') %>% 
    dplyr::rename(observed = obs, predicted = model) %>% 
    tibble::as_tibble()
  #
  ## Maturity
  ## TO-DO ADD LOWER AND UPPER
  if (any('matp' %in% dat$type)){
    
    stockdist <- 
      dat %>% 
      dplyr::filter(type == 'matp') %>%
      dplyr::group_by(year, step, area, length, age, name) %>%
      dplyr::mutate(pred.ratio = predicted / sum(predicted, na.rm = TRUE),
                    obs.ratio = observed / sum(observed, na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(name, year, step, area, stock, length, age, fleetnames, 
                    observed, obs.ratio, predicted, pred.ratio)
    
  }else{
    stockdist <- NULL
  }
  
  ## Age and Length distributions
  if (any(c('ldist','aldist') %in% dat$type)){
    
    catchdist.fleets <- 
      dat %>% 
      dplyr::filter(type %in% c('ldist', 'aldist')) %>% 
      dplyr::group_by(year, step, area, name) %>%
      dplyr::mutate(total.catch = sum(observed, na.rm = TRUE),
                    total.pred = sum(predicted, na.rm = TRUE),
                    obs = observed,
                    pred = predicted,
                    observed = observed / sum(observed, na.rm = TRUE),
                    predicted = predicted / sum(predicted, na.rm = TRUE),
                    residuals = ifelse(observed == 0, NA, observed - predicted)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(name, year, step, area, stock, length, age, fleetnames, 
                    obs, total.catch, observed,
                    pred, total.pred, predicted, residuals)
    
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
      dplyr::mutate(index = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\2', comp),
                    type = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\3', comp),
                    fleet = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\4', comp),
                    origin = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\5', comp),
                    name = gsub('(adist)_([A-Za-z]+)_([A-Za-z]+)_(.+)_(model|obs)__num', '\\2.\\4', comp),
                    length = gsub('len', '', length) %>% as.numeric(),
                    area = as.numeric(area)) %>%
      extract_year_step() %>%
      dplyr::select(-comp) %>%
      tidyr::pivot_wider(names_from = origin, values_from = Freq) %>%
      dplyr::mutate(id = paste(index, type, fleet, sep = '_')) %>% 
      dplyr::left_join(tmp[grepl('^adist_.+__params$', names(tmp))] %>% 
                         dplyr::bind_rows(.id = 'id') %>%
                         dplyr::mutate(id = gsub('^adist_|_model__params$', '', id))) %>% 
      dplyr::rename(observed = obs, number = model, intercept = alpha, slope = beta) %>% 
      dplyr::mutate(predicted = ifelse(type == 'log', 
                                       exp(intercept) * number^slope,
                                       intercept + slope * number)) %>% 
      dplyr::select(name, year, step, area, length, fleet, 
                    index, type, intercept, slope, observed, number, predicted) 
      
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
      dplyr::mutate(stock = gsub('(.+)_(history__suit)_(.+)', '\\1', comp),
                    fleet = gsub('(.+)_(history__suit)_(.+)', '\\3', comp),
                    area = as.numeric(area),
                    length = gsub('len','',length) %>% as.numeric(),
                    age = gsub('age','',age) %>% as.numeric()) %>%
      extract_year_step() %>% 
      dplyr::rename(suit = Freq) %>%
      dplyr::select(year, step, area, stock, length, age, fleet, suit) %>%
      tibble::as_tibble()
    
  }else{
    suitability <- NULL
  }
  
  ## --------------------------------------------------------------------------------
  
  ## Likelihood
  if (any(grepl('^nll_', names(tmp)))){
    
    likelihood <-
      tmp[grep('^nll_', names(tmp))] %>%
      purrr::map(~tibble(time=names(.), lik_score=as.numeric(.))) %>% 
      dplyr::bind_rows(.id='lik_comp') %>% 
      dplyr::filter(!grepl('understocking', lik_comp)) %>%
      dplyr::mutate(type = gsub('.+(wgt|num|weight)','\\1',lik_comp),
                    component = gsub('nll_(cdist|adist)_([A-Za-z]+)_(.+)__(num|weight)', '\\3', lik_comp),
                    data_type = gsub('nll_(cdist|adist)_([A-Za-z]+)_(.+)__(num|weight)', '\\1_\\2', lik_comp)) %>%
      select(-lik_comp) %>%
      tidyr::pivot_wider(names_from=type, values_from=lik_score) %>%
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
      dplyr::mutate(stock = gsub('(.+)_history__renewalnum$', '\\1', comp),
                    age = gsub('age', '', age) %>% as.numeric()) %>% 
      extract_year_step() %>%
      dplyr::filter(age == min(age),
                    step %in% rec.steps) %>% ## ADD Recruit-at-age & and min(age) should be taken from stock attributes
      dplyr::group_by(stock, year, step, area, age) %>% 
      dplyr::summarise(recruitment = sum(Freq)) %>% 
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
    dplyr::mutate(stock = gsub('(.+)_history__(.+)', '\\1', comp),
                  fleet = gsub('(.+)_history__(.+)', '\\2', comp)) %>% 
    dplyr::select(-comp) %>% 
    
    left_join(
      tmp[grepl('.history__wgt', names(tmp))] %>% 
        purrr::map(as.data.frame.table, stringsAsFactors = FALSE, responseName = 'weight') %>% 
        dplyr::bind_rows(.id='comp') %>% 
        dplyr::mutate(stock = gsub('(.+)_history__wgt', '\\1', comp)) %>% 
        dplyr::select(-comp) %>% 
        dplyr::left_join(
          
          tmp[grepl('.history__num', names(tmp))] %>% 
            purrr::map(as.data.frame.table, stringsAsFactors = FALSE, responseName = 'abundance') %>% 
            dplyr::bind_rows(.id='comp') %>% 
            dplyr::mutate(stock = gsub('(.+)_history__num', '\\1', comp)) %>% 
            dplyr::select(-comp)
          )
      ) %>% 
    extract_year_step() %>% 
    dplyr::mutate(length = gsub('len', '', length) %>% as.numeric(),
                  number_consumed = ifelse(biomass_consumed == 0, 0, biomass_consumed / weight)) %>% 
    as_tibble()
      
  ## Stock full
  stock.full <- 
    all_reports %>% 
    dplyr::group_by(stock, year, step, area, length) %>% 
    dplyr::summarise(number = sum(abundance), 
                     mean_weight = mean(weight)) %>% 
    dplyr::ungroup()
  
  ## Stock std
  stock.std <- 
    all_reports %>% 
    dplyr::group_by(stock, year, step, area, age) %>%
    dplyr::summarise(number = sum(abundance),
                     mean_length = mean(length),
                     stddev_length = sd(length),
                     mean_weight = mean(weight)) %>% 
    dplyr::ungroup()
  
  ## Stock prey
  stock.prey <- 
    all_reports %>%
    dplyr::group_by(stock, year, step, area, age) %>%
    dplyr::summarise(number = sum(abundance),
                     number_consumed = sum(number_consumed),
                     biomass_consumed = sum(biomass_consumed)) %>% 
    dplyr::mutate(mortality = -log(1 - number_consumed / number)/0.25) %>% 
    dplyr::ungroup()
  
  ## Predator prey
  predator.prey <- 
    all_reports %>% 
    dplyr::group_by(stock, fleet, year, step, area, length) %>% 
    dplyr::summarise(number_consumed = sum(number_consumed),
                     biomass_consumed = sum(biomass_consumed),
                     mortality = -log(1 - sum(number_consumed) / sum(abundance))/0.25) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(stock, fleet, year, step, area) %>% 
    dplyr::mutate(suit = mortality / max(mortality),
                  suit = ifelse(is.finite(suit), suit, 0)) %>%
    dplyr::rename(predator = fleet, prey = stock)
  
  ## Fleet info
  fleet.catches <- 
    predator.prey %>% 
    dplyr::group_by(year, area, predator, prey) %>% 
    dplyr::summarise(amount = sum(biomass_consumed)) %>%
    dplyr::rename(fleet = predator, stock = prey)
  
  fleet.info <- 
    stock.full %>%
    dplyr::left_join(predator.prey %>% 
                       dplyr::select(year,
                                     step, 
                                     area,
                                     fleet = predator, 
                                     stock = prey, 
                                     length, 
                                     suit),
                     by = c('stock', 'year', 'step', 'area', 'length')) %>%
    dplyr::group_by(year, step, area, fleet) %>%
    dplyr::summarise(harv.bio = sum(suit * number * mean_weight)) %>%
    dplyr::left_join(fleet.catches %>% 
                       dplyr::group_by(year, fleet, area) %>% 
                       dplyr::summarise(amount = sum(amount)),
                     by = c('year', 'area', 'fleet')) %>%
    dplyr::group_by(year, step, area, fleet) %>%
    dplyr::mutate(amount = ifelse(is.na(amount), 0, amount),
                  harv.rate = amount/harv.bio)
    
    ## Suitability over fleets                 
    harv.suit <- 
      predator.prey %>%
      dplyr::group_by(year, step, prey, length) %>%
      dplyr::filter(biomass_consumed > 0) %>%
      dplyr::summarise(suit = sum(biomass_consumed * suit) / sum(biomass_consumed)) %>%
      dplyr::rename(stock = prey)
  
    ## Annual F
    ## TO-DO: Add weighted mortality
    f.by.year <- 
      stock.prey %>%
      dplyr::group_by(stock, year, area) %>%
      dplyr::summarise(catch = sum(biomass_consumed),
                       num.catch = sum(number_consumed),
                       F = mean(mortality, na.rm = TRUE))
    
   
    
    ## Annual output
    res.by.year <- 
      stock.full %>%
      dplyr::filter(step %in% steps) %>%
      dplyr::left_join(harv.suit,
                       by = c('stock', 'year', 'step', 'length')) %>%
      dplyr::group_by(stock, year, area, step) %>%
      dplyr::summarise(total.number = sum(number),
                       total.biomass = sum(number * mean_weight),
                       harv.biomass = sum(number * suit * mean_weight)) %>%
      dplyr::left_join(f.by.year,
                       by = c('stock', 'year', 'area')) %>% 
      dplyr::left_join(stock.recruitment %>% 
                         dplyr::group_by(stock, year, area) %>% 
                         dplyr::summarise(recruitment = sum(recruitment)),
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

g3a_report_history <- function(actions, vars = '__num$|__wgt$'){
  
  ## Extract stock and fleet objects from collated actions
  stocks <- find_stocks_fleets(name = 'stock', env = attr(actions, '.Env'))
  fleets <- find_stocks_fleets(name = 'fleet_stock', env = attr(actions, '.Env'))
  
  cat('Compiling model fit for:', 
      '\n\nstocks = ', paste(names(stocks), collapse = ', '),
      '\nfleets = ', paste(names(fleets), collapse = ', '), '\n\n')
  
  ## Loop over stocks & setup reporting actions
  report_actions_fit <- list()
  for (i in names(stocks)){
    
    ## Assign stock object to current environment
    assign(i, stocks[[i]])
    
    ## Setup report actions
    ## TODO: make easier to report specific variables (using vars above)
    for (j in paste0(i, '__', c('num', 'wgt', 
                                'renewalnum', 'renewalwgt', 
                                names(fleets), 
                                paste0('suit_', names(fleets))))){
      
      ## Check whether the stock instance exists
      if (!exists(j, attr(actions, '.Env'))) next
    
      report_actions_fit <- c(report_actions_fit, 
                              list(
                                g3a_report_stock(report_stock =
                                                   g3s_clone(inner_stock = stocks[[i]], var_name = paste0(i, '_history')) %>% 
                                                   g3s_time(year = seq(get('start_year', envir = attr(actions, '.Env')),
                                                                       get('end_year', envir = attr(actions, '.Env'))),
                                                            step = 1:length(get('step_lengths', envir = attr(actions, '.Env')))),
                                                 input_stock = stocks[[i]],
                                                 report_f = as.formula(sprintf('~stock_ss(%s)', j))
                                                 )
                                )
      )
    }
  }
  return(report_actions_fit)
}

extract_year_step <- function(data){
  
  data %>% 
    tidyr::extract(col = 'time', 
                   into = c('year', 'step'), 
                   regex="^(\\d+)-(\\d+)$", convert=TRUE) %>% 
    return()
  
}

