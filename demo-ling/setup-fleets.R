## Collect catches by fleet:
lln_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


bmt_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


gil_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

foreign_landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))

igfs_landings <- 
  structure(expand.grid(year=defaults$year,step=2,area=1,total_weight=1),
            area_group = mfdb_group(`1` = 1))


## create fleet actions
fleet_actions <- 
  list(
    lln %>% 
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~g3_param('ling.lln.alpha'), ~g3_param('ling.lln.l50')),
                               ling_mat = g3_suitability_exponentiall50(~g3_param('ling.lln.alpha'), ~g3_param('ling.lln.l50'))),
                             amount_f = g3_timeareadata('lln_landings', lln_landings[[1]] %>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    bmt %>% 
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~g3_param('ling.bmt.alpha'), ~g3_param('ling.bmt.l50')),
                               ling_mat = g3_suitability_exponentiall50(~g3_param('ling.bmt.alpha'), ~g3_param('ling.bmt.l50'))),
                             amount_f = g3_timeareadata('bmt_landings', bmt_landings[[1]]%>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    gil %>% 
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~g3_param('ling.gil.alpha'), ~g3_param('ling.gil.l50')),
                               ling_mat = g3_suitability_exponentiall50(~g3_param('ling.gil.alpha'), ~g3_param('ling.gil.l50'))),
                             amount_f = g3_timeareadata('gil_landings', gil_landings[[1]]%>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    foreign  %>% 
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~g3_param('ling.lln.alpha'), ~g3_param('ling.lln.l50')),
                               ling_mat = g3_suitability_exponentiall50(~g3_param('ling.lln.alpha'), ~g3_param('ling.lln.l50'))),
                             amount_f = g3_timeareadata('foreign_landings', foreign_landings[[1]]%>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    
    igfs %>% 
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50')),
                               ling_mat = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
                             amount_f = g3_timeareadata('igfs_landings', igfs_landings%>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))))
    