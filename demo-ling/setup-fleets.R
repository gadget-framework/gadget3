
igfs_landings <-
  structure(expand.grid(year=defaults$year,step=2,area=1,total_weight=1),
            area_group = mfdb_group(`1` = 1))


l50 <-
  #~avoid_zero(fleet_l50)
  ~bounded(fleet_l50,40,100)


## create fleet actions
fleet_actions <-
  list(
    lln %>%
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~bounded(g3_param('ling.lln.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.lln.l50')))),
                               ling_mat = g3_suitability_exponentiall50(~bounded(g3_param('ling.lln.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.lln.l50'))))),
                             amount_f = g3_timeareadata('lln_landings', lln_landings[[1]] %>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    bmt %>%
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~bounded(g3_param('ling.bmt.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.bmt.l50')))),
                               ling_mat = g3_suitability_exponentiall50(~bounded(g3_param('ling.bmt.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.bmt.l50'))))),
                             amount_f = g3_timeareadata('bmt_landings', bmt_landings[[1]]%>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    gil %>%
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~bounded(g3_param('ling.gil.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.gil.l50')))),
                               ling_mat = g3_suitability_exponentiall50(~bounded(g3_param('ling.gil.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.gil.l50'))))),
                             amount_f = g3_timeareadata('gil_landings', gil_landings[[1]]%>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    foreign  %>%
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~bounded(g3_param('ling.lln.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.lln.l50')))),
                               ling_mat = g3_suitability_exponentiall50(~bounded(g3_param('ling.lln.alpha'),0.01,1),
                                                                        gadget3:::f_substitute(l50,list(fleet_l50=~g3_param('ling.lln.l50'))))),
                             amount_f = g3_timeareadata('foreign_landings', foreign_landings[[1]]%>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),

    igfs %>%
      g3a_predate_totalfleet(list(ling_imm, ling_mat),
                             suitabilities = list(
                               ling_imm = g3_suitability_exponentiall50(~bounded(g3_param('ling.igfs.alpha'),0.01,1),
                                                                        ~bounded(g3_param('ling.igfs.l50'),20,50)),
                               ling_mat = g3_suitability_exponentiall50(~bounded(g3_param('ling.igfs.alpha'),0.01,1),
                                                                        ~bounded(g3_param('ling.igfs.l50'),20,50))),
                             amount_f = g3_timeareadata('igfs_landings', igfs_landings%>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))))
