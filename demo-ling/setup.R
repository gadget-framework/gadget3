library(mfdb)
library(tidyverse)
library(gadget3)

mdb<-mfdb('Iceland')
year_range <- 1982:lubridate::year(Sys.Date())

reitmapping <- 
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

defaults <- list(
  area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb_timestep_quarterly,
  year = year_range,
  species = 'LIN')

time <-  g3a_time(start_year = min(defaults$year), 
                  end_year = max(defaults$year), 
                  defaults$timestep)
bootstrap <- FALSE

areas <- structure(
    seq_along(defaults$area),
    names = names(defaults$area))

# Setup up the model blue print
## modelled stocks

ling_imm <- 
  g3_stock('ling_imm', seq(20, 156, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(3, 10)

ling_mat <- 
  g3_stock('ling_mat', seq(20, 156, 4)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(5, 15)

## fleets

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

igfs <- 
  g3_fleet('igfs') %>% 
  g3s_livesonareas(areas[c('1')])


source('demo-ling/setup-fleets.R')
source('demo-ling/setup-model.R')
source('demo-ling/setup-catchdistribution.R')
source('demo-ling/setup-indices.R')
source('demo-ling/setup-likelihood.R')

## set up reporting:

imm_report <- g3s_clone(ling_imm, 'imm_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

mat_report <- g3s_clone(ling_mat, 'mat_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

ling_model <- g3_to_r(c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  list(gadget3:::g3a_report_stock(imm_report,ling_imm,'num'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'num'),
       gadget3:::g3a_report_stock(imm_report,ling_imm,'wgt'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'wgt'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'igfs'),
       gadget3:::g3a_report_stock(imm_report,ling_mat,'igfs'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'bmt'),
       gadget3:::g3a_report_stock(imm_report,ling_mat,'bmt'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'lln'),
       gadget3:::g3a_report_stock(imm_report,ling_mat,'lln'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'gil'),
       gadget3:::g3a_report_stock(imm_report,ling_mat,'gil'),
       gadget3:::g3a_report_stock(mat_report,ling_mat,'foreign'),
       gadget3:::g3a_report_stock(imm_report,ling_mat,'foreign')), 
  list(time)))


tmb_ling <- g3_to_tmb(c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  list(time)))


## update the input parameters with sane initial guesses
tmb_param <- attr(tmb_ling, 'parameter_template')

## I would like to do something like this:
if(FALSE){
  read.gadget.parameters(sprintf('%s/params.out',gd)) %>% 
    init_guess('rec.[0-9]|init.[0-9]',1,0.001,1000,1) %>%
    init_guess('recl',12,1,20,1) %>% 
    init_guess('rec.sd',5, 4, 20,1) %>% 
    init_guess('Linf',160, 100, 200,1) %>% 
    init_guess('k$',90, 40, 100,1) %>% 
    init_guess('bbin',6, 1e-08, 100, 1) %>% 
    init_guess('alpha', 0.5,  0.01, 3, 1) %>% 
    init_guess('l50',50,10,100,1) %>% 
    init_guess('walpha',lw.constants$estimate[1], 1e-10, 1,0) %>% 
    init_guess('wbeta',lw.constants$estimate[2], 2, 4,0) %>% 
    init_guess('M$',0.15,0.001,1,0) %>% 
    init_guess('rec.scalar',400,1,500,1) %>% 
    init_guess('init.scalar',200,1,300,1) %>% 
    init_guess('mat2',mat.l50$l50,0.75*mat.l50$l50,1.25*mat.l50$l50,1) %>% 
    init_guess('mat1',70,  10, 200, 1) %>% 
    init_guess('init.F',0.4,0.1,1,1) %>% 
    init_guess('p0',0,0,1,1) %>% 
    init_guess('p2',1,0,1,1) %>% 
    init_guess('p3',1,0.01,100,1) %>% 
    init_guess('p4',1,0.01,100,1) %>% 
    init_guess('mode',70,30,90,1) %>% 
    write.gadget.parameters(.,file=sprintf('%s/params.in',gd))
}

## but lets settle for 
param_names <- rownames(tmb_param)

source('demo-ling/setup-initial_parameters.R')

ling_param <- list(  # ./06-ling/12-new_ass/params.in
  "ling.Linf" = 160,
  "ling.k" = 90,
  "lingimm.walpha" = 2.27567436711055e-06,
  "lingimm.wbeta" = 3.20200445996187,
  "ling.bbin" = 6,
  "lingimm.M" = rep(0.15,3,10),
  "lingimm.init.scalar" = 200,
  "ling.init.F" = 0.4,
  "lingimm.init" = rep(1, 10 - 3 + 1),
  "ling.recl" = 12,
  "ling.mat1" = 70,
  "ling.mat2" = 75,
  "ling.rec.scalar" = 400,
  "ling.rec.1982" = 1,
  "ling.rec.sd" = 5,
  "ling.rec" = rep(1, 2020 - 1981 + 1),
  "lingmat.M" = rep(0.15,5,15),
  "lingmat.init.scalar" = 200,
  "lingmat.init" = rep(1, 15 - 5 + 1),
  "lingmat.walpha" = 2.27567436711055e-06,
  "lingmat.wbeta" = 3.20200445996187,
  "ling.igfs.alpha" = 0.5,
  "ling.igfs.l50" = 50,
  "ling.lln.alpha" = 0.5,
  "ling.lln.l50" = 50,
  "ling.bmt.alpha" = 0.5,
  "ling.bmt.l50" = 50,
  "ling.gil.alpha" = 0.5,
  "ling.gil.l50" = 50,
  "ling.init.sd" = init.sigma$ms,
  "ling_si_alpha1" = 6.4e-9,
  "ling_si_beta1" = 1.4,
  'ling_si_alpha2' = 1.7e-7,
  "ling_si_beta2" = 1.3,
  'ling_si_alpha3' = 2.7e-5,
  'ling_si_alpha4' = 4.2e-5,
  'ling_si_alpha5' = 5.4e-5,
  'ling_si_alpha6' = 5.8e-5,
  'ling_si_alpha7' = 7.1e-5,
  end = NULL)

#ling_model <- edit(ling_model)
result <- ling_model(ling_param)

environment(ling_model)$model_report -> tmp

tmb_param$value <- I(ling_param[rownames(tmb_param)])
ling_model_tmb <- g3_tmb_adfun(tmb_ling,tmb_param)

ling_model_tmb$fn(g3_tmb_par(tmb_param))

params <- 
  tibble(switch = names(par), value = par, upper = 2*par, lower = 0.5*par, optimise = 0*par + 1, order = 1:length(par)) #%>% 
  # Rgadget::init_guess('M[0-9+]$',optimise = 0) %>% 
  # Rgadget::init_guess('__sd[0-9+]$',optimise = 0) %>% 
  # Rgadget::init_guess('si_alpha[0-9+]$',optimise = 0) %>% 
  # Rgadget::init_guess('si_beta[0-9]$',optimise = 0) %>% 
  # arrange(order)
  #%>% 
#  mutate(upper = ifelse(optimise == 0, 1.01*value,upper),
#         lower = ifelse(optimise == 0, 0.99*value,lower))

fit <- nlminb(fit$par, ling_model_tmb$fn, ling_model_tmb$gr, 
              upper = params$upper,
              lower = params$lower, control = list(abs.tol = 1e-20,xf.tol = 0.1)) 
