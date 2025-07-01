library(magrittr)
library(unittest)
library(gadget3)

areas <- c('1' = 1)
year_range <- 1952:1953
timestep <- c(3,3,3,3)

time_actions <- list(
  g3a_time(start_year = min(year_range),
           end_year = max(year_range),
           timestep),
  # NB: Only required for testing
  gadget3:::g3l_test_dummy_likelihood(),
  list())

stock <-
  g3_stock('st', seq(10, 20, 5)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(minage = 0, maxage = 1)

stock_actions <-
  list(
    g3a_initialconditions_normalparam(stock),
    g3a_age(stock),
    NULL
  )

survey <-
  g3_fleet('survey') %>%
  g3s_livesonareas(areas[c('1')])

survey_landings <- expand.grid(year = year_range, step = 1:4, area = 1)
survey_landings$total_weight <- 1

numberfleet_actions <-
  list(
    g3a_predate_fleet(survey,
                      list(stock),
                      suitabilities =  g3_suitability_exponentiall50(g3_parameterized('alpha'),
                                                                     g3_parameterized('l50')),
                      catchability_f = g3a_predate_catchability_numberfleet(10)
                      )
  )

actions <- c(time_actions, stock_actions, numberfleet_actions)

model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("g3a_predate_catchability_numberfleet", {
    params <- attr(model_fn, 'parameter_template')
    params[] <- 1
    params$recage <- 0
    params$retro_years <- params$project_years <- 0
    
    r <- model_fn(params)
 
    # st__wgt was being zero'ed by g3a_age(), check this doesn't break again.
    ok(all(!is.nan(attr(r, 'st__num'))), "st__num: Hasn't gone NaN")
    ok(all(!is.nan(attr(r, 'st__wgt'))), "st__wgt: Hasn't gone NaN")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params, g3_test_tmb = 2)
})
