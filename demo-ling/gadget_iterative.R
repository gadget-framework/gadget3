g3_weights <-
  out[grepl('nll_cdist_.+__num',names(out))] %>%
  map(sum) %>%
  map(as_tibble) %>%
  bind_rows(.id = 'comp') %>%
  mutate(comp = gsub('nll_cdist_(.+)__num','\\1',comp)) %>%
  left_join(list(aldist_bmt=aldist.bmt,
                 aldist_gil=aldist.gil,
                 aldist_lln=aldist.lln,
                 aldist_igfs=aldist.igfs,
                 ldist_bmt=ldist.bmt,
                 ldist_gil=ldist.gil,
                 ldist_lln=ldist.lln,
                 ldist_igfs=ldist.igfs,
                 matp_igfs=matp.igfs,
                 si_igfs_si1=igfs.SI1,
                 si_igfs_si2a=igfs.SI2a,
                 si_igfs_si2b=igfs.SI2b,
                 si_igfs_si3a=igfs.SI3a,
                 si_igfs_si3b=igfs.SI3b,
                 si_igfs_si3c=igfs.SI3c,
                 si_igfs_si3d=igfs.SI3d) %>%
              map(~nrow(.[[1]])) %>%
              map(as_tibble) %>%
              bind_rows(.id = 'comp'),
            by = 'comp') %>%
  mutate(init_weight = 1/value.x,
         group = case_when(comp %in% c('si_igfs_si1','si_igfs_si2a','si_igfs_si2b') ~ 'S1',
                           grepl('si_igfs_si3',comp) ~ 'S2',
                           grepl('bmt|gil',comp) ~ 'comm',
                           TRUE~comp))



tmp_func <- function(sel_comp = NULL){
  g3_weights <- g3_weights %>% mutate(init_weight = ifelse(comp %in% sel_comp,1000*init_weight,init_weight))
  ling_param[paste0(g3_weights$comp,'_weight')] <- g3_weights$init_weight


  # Get the parameter template to fill in
  tmb_param <- attr(tmb_ling, 'parameter_template')

  # Copy initial guesses from R model
  tmb_param$value <- I(ling_param[rownames(tmb_param)])

  # Configure lower and upper bounds
  tmb_param$lower <- vapply(tmb_param$value, function (x) 0.5 * x[[1]], numeric(1))
  tmb_param$upper <- vapply(tmb_param$value, function (x) 2 * x[[1]], numeric(1))
  #tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$lower <- 0.0001
  #tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$upper <- 1e3
  tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
  tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$upper <- 1e3
  tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
  tmb_param[grepl('^ling\\.init\\.', rownames(tmb_param)),]$upper <- 1e3

  # Disable optimisation for some parameters, to make life easier
  tmb_param[c('lingimm.walpha',
              'lingimm.wbeta',
              'lingmat.walpha',
              #"ling_si_alpha1",
              #"ling_si_beta1" ,
              #'ling_si_alpha2',
              #"ling_si_beta2" ,
              #'ling_si_alpha3',
              #'ling_si_alpha4',
              #'ling_si_alpha5',
              #'ling_si_alpha6',
              #'ling_si_alpha7',
              'lingmat.wbeta'),]$optimise <- FALSE
  tmb_param[grepl('^lingimm\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
  tmb_param[grepl('^lingmat\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
  tmb_param[grepl('^lingimm\\.init\\.sd', rownames(tmb_param)),]$optimise <- FALSE
  tmb_param[grepl('^lingmat\\.init\\.sd', rownames(tmb_param)),]$optimise <- FALSE
  tmb_param[grepl('weight$', rownames(tmb_param)),]$optimise <- FALSE


  # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
  ling_model_tmb <- g3_tmb_adfun(tmb_ling,tmb_param)

  fit.opt <- optim(g3_tmb_par(tmb_param),
                   ling_model_tmb$fn,
                   ling_model_tmb$gr,
                   method = 'BFGS',
                   control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))
  return(fit.opt)
}

res_opt <-
  g3_weights %>%
  split(.$group) %>%
  parallel::mclapply(tmp_func)
