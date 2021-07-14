## -----------------------------------------------------------------------------
##
## Setup likelihood 
##
## -----------------------------------------------------------------------------

## weird inconsistencies in Gadget
aldist.igfs[[1]]$step <- 2
ldist.igfs[[1]]$step <- 2
matp.igfs[[1]]$step <- 2

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

ling_likelihood_actions <- list(
  g3l_understocking(stock_list, nll_breakdown = nll_breakdown, weight = 1e6),
  
  g3l_catchdistribution(
    'ldist_lln',
    ldist.lln[[1]] %>% ## tow == 60228 was wrongly assigned, omit samples from that quarter
      filter(!(year==1993&step==4)),
    fleets = list(lln),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'aldist_lln',
    aldist.lln[[1]] %>%  ## only 20 fish aged taken in those quarters
      filter(year>1998,!((year==2002|year==2003)&step==2)),
    fleets = list(lln),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  g3l_catchdistribution(
    'ldist_bmt',
    (ldist.bmt[[1]]) %>% ## to few samples (<=20 fish lengths)
      filter(!(year==1982&step==4),
             !(year==1984&step==1),
             !(year==1992&step==4),
             !(year==1994&step==1),
             !(year==1998&step==3),
             !(year==1989&step==3)),
    fleets = list(bmt),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_catchdistribution(
    'aldist_bmt',
    (aldist.bmt[[1]]) %>%
      filter(year>1998),
    fleets = list(bmt),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_catchdistribution(
    'ldist_gil',
    (ldist.gil[[1]]) %>% ## only one fish lengthmeasured
      filter(!(year==2005&step==2)),
    fleets = list(gil),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_catchdistribution(
    'aldist_gil',
    (aldist.gil[[1]]) %>%
      filter(year>1998),
    fleets = list(gil),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_catchdistribution(
    'ldist_igfs',
    (ldist.igfs[[1]]),
    fleets = list(igfs),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_catchdistribution(
    'aldist_igfs',
    (aldist.igfs[[1]]) %>% ## only two age samples in 1989
      filter(year>1998),
    fleets = list(igfs),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_catchdistribution(
    'matp_igfs',
    (matp.igfs[[1]] %>%
       rename(stock = maturity_stage) %>%
       mutate(stock = recode(as.factor(stock), lingimm = "ling_imm", lingmat = "ling_mat"))),
    fleets = list(igfs),
    stocks = stock_list,
    g3l_distribution_sumofsquares(),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si1',
    (igfs.SI1[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha1", sep0="_"),
                                       beta = g3_stock_param(species_name, "si_beta1", sep0="_")),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si2a',
    (igfs.SI2a[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha2", sep0="_"),
                                       beta = g3_stock_param(species_name, "si_beta2", sep0="_")),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si2b',
    (igfs.SI2b[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha3", sep0="_"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si3a',
    (igfs.SI3a[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha4", sep0="_"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si3b',
    (igfs.SI3b[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha5", sep0="_"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si3c',
    (igfs.SI3b[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha6", sep0="_"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  g3l_distribution(
    'si_igfs_si3d',
    (igfs.SI3d[[1]]),
    fleets = list(),
    stocks = stock_list,
    g3l_distribution_surveyindices_log(alpha = g3_stock_param(species_name, "si_alpha7", sep0="_"),
                                       beta = 1),
    nll_breakdown = nll_breakdown,     report = lik_report),
  
  list()
)
