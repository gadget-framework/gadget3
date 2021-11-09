## -----------------------------------------------------------------------------
##
## Fleet actions:
##
## -----------------------------------------------------------------------------

## Survey(s)
igfs <-
  g3_fleet('igfs') %>%
  g3s_livesonareas(areas[c('1')])

## Commercial
lln <-
  g3_fleet('lln') %>%
  g3s_livesonareas(areas[c('1')])

bmt <-
  g3_fleet('bmt') %>%
  g3s_livesonareas(areas[c('1')])

gil <-
  g3_fleet('gil') %>%
  g3s_livesonareas(areas[c('1')])

foreign <-
  g3_fleet('foreign') %>%
  g3s_livesonareas(areas[c('1')])

## Bounded parameters for fleet suitabilities
fleet_bounds <- list(
  
  'lln.l50' = list(lower = 40, upper = 100),
  'lln.alpha' = list(lower = 0.01, upper = 1),
  
  'bmt.l50' = list(lower = 40, upper = 100),
  'bmt.alpha' = list(lower = 0.01, upper = 1),
  
  'gil.l50' = list(lower = 40, upper = 100),
  'gil.alpha' = list(lower = 0.01, upper = 1),
  
  'igfs.l50' = list(lower = 20, upper = 50),
  'igfs.alpha' = list(lower = 0.01, upper = 1)
  
)

## create fleet actions
fleet_actions <-
  list(
    lln %>%
      g3a_predate_fleet(list(ling_imm, ling_mat),
                        suitabilities = list(
                          ling_imm = g3_suitability_exponentiall50(bounded_param(ling_imm, "lln.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_imm, "lln.l50", fleet_bounds, id = 'species')),
                          ling_mat = g3_suitability_exponentiall50(bounded_param(ling_mat, "lln.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_mat, "lln.l50", fleet_bounds, id = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    bmt %>%
      g3a_predate_fleet(list(ling_imm, ling_mat),
                        suitabilities = list(
                          ling_imm = g3_suitability_exponentiall50(bounded_param(ling_imm, "bmt.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_imm, "bmt.l50", fleet_bounds, id = 'species')),
                          ling_mat = g3_suitability_exponentiall50(bounded_param(ling_mat, "bmt.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_mat, "bmt.l50", fleet_bounds, id = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('bmt_landings', bmt_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    gil %>%
      g3a_predate_fleet(list(ling_imm, ling_mat),
                        suitabilities = list(
                          ling_imm = g3_suitability_exponentiall50(bounded_param(ling_imm, "gil.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_imm, "gil.l50", fleet_bounds, id = 'species')),
                          ling_mat = g3_suitability_exponentiall50(bounded_param(ling_mat, "gil.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_mat, "gil.l50", fleet_bounds, id = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('gil_landings', gil_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    foreign  %>%
      g3a_predate_fleet(list(ling_imm, ling_mat),
                        suitabilities = list(
                          ling_imm = g3_suitability_exponentiall50(bounded_param(ling_imm, "lln.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_imm, "lln.l50", fleet_bounds, id = 'species')),
                          ling_mat = g3_suitability_exponentiall50(bounded_param(ling_mat, "lln.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_mat, "lln.l50", fleet_bounds, id = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('foreign_landings', foreign_landings[[1]] %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))),
    
    igfs %>%
      g3a_predate_fleet(list(ling_imm, ling_mat),
                        suitabilities = list(
                          ling_imm = g3_suitability_exponentiall50(bounded_param(ling_imm, "igfs.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_imm, "igfs.l50", fleet_bounds, id = 'species')),
                          ling_mat = g3_suitability_exponentiall50(bounded_param(ling_mat, "igfs.alpha", fleet_bounds, id = 'species'),
                                                                   bounded_param(ling_mat, "igfs.l50", fleet_bounds, id = 'species'))),
                        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_landings', igfs_landings %>%
                                                                                               mutate(area = as.numeric(area),
                                                                                                      step = as.numeric(step),
                                                                                                      year = as.numeric(year))))))
