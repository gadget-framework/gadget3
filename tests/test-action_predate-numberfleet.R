library(magrittr)
library(unittest)
library(gadget3)

areas <- c('1' = 1)
year_range <- 1952:1953
timestep <- c(3,3,3,3)

time_actions <- list(
  # Keep TMB happy
  g3_formula( nll <- nll + g3_param("dummy", value = 0) ),
  g3a_time(start_year = min(year_range),
           end_year = max(year_range),
           timestep),
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
model_cpp <- g3_to_tmb(attr(model_fn, 'actions'), trace = FALSE)
if (Sys.getenv('G3_TEST_TMB') == "2") {
    #model_cpp <- edit(model_cpp)
    #writeLines(TMB::gdbsource(g3_tmb_adfun(model_cpp, compile_flags = "-g", output_script = TRUE)))
    model_tmb <- g3_tmb_adfun(model_cpp, trace = TRUE, compile_flags = c("-O0", "-g"))
}

ok_group("g3a_predate_catchability_numberfleet", {
    params <- attr(model_fn, 'parameter_template')
    params[] <- 1
    params$recage <- 0
    params$retro_years <- params$project_years <- 0
    
    r <- model_fn(params)
 
    # st__wgt was being zero'ed by g3a_age(), check this doesn't break again.
    ok(all(!is.nan(attr(r, 'st__num'))), "st__num: Hasn't gone NaN")
    ok(all(!is.nan(attr(r, 'st__wgt'))), "st__wgt: Hasn't gone NaN")

    if (Sys.getenv('G3_TEST_TMB') == "2") gadget3:::ut_tmb_r_compare(model_fn, model_tmb, params, model_cpp = model_cpp)
})
