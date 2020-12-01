library(mfdb)
library(tidyverse)
library(gadget3)


year_range <- 1982:lubridate::year(Sys.Date())
read_data <- FALSE

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


if(read_data){
  mdb<-mfdb('Iceland')
  source('demo-ling/setup-fleet-data.R')
  source('demo-ling/setup-catchdistribution.R')
  source('demo-ling/setup-indices.R')
  source('demo-ling/setup-initial_parameters.R')
} else {
  fs::dir_ls('data') %>% 
    stringr::str_subset('.Rdata') %>% 
    lapply(load,.GlobalEnv)
}

source('demo-ling/setup-fleets.R')
source('demo-ling/setup-model.R')
source('demo-ling/setup-likelihood.R')

## set up reporting:

imm_report <- g3s_clone(ling_imm, 'imm_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

mat_report <- g3s_clone(ling_mat, 'mat_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

report_actions <- list(
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__num)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__wgt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__igfs)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__igfs)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__bmt)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__bmt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__lln)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__lln)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__gil)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__gil)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__foreign)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__foreign)))

ling_model <- g3_to_r(c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  report_actions,
  list(time)), strict = TRUE)

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

ling_param <- tmb_param$value
ling_param[["ling.Linf"]] <- 160
ling_param[["ling.k"]] <- 90
ling_param[["lingimm.walpha"]] <- 2.27567436711055e-06
ling_param[["lingimm.wbeta"]] <- 3.20200445996187
ling_param[["ling.bbin"]] <- 6
ling_param[["lingimm.init.scalar"]] <- 200
ling_param[["ling.init.F"]] <- 0.4
ling_param[["ling.recl"]] <- 12
ling_param[["ling.mat1"]] <- 70
ling_param[["ling.mat2"]] <- 75
ling_param[["ling.rec.scalar"]] <- 400
ling_param[["ling.rec.1982"]] <- 1
ling_param[["ling.rec.sd"]] <- 5
ling_param[["lingmat.init.scalar"]] <- 200
ling_param[["lingmat.walpha"]] <- 2.27567436711055e-06
ling_param[["lingmat.wbeta"]] <- 3.20200445996187
ling_param[["ling.igfs.alpha"]] <- 0.5
ling_param[["ling.igfs.l50"]] <- 50
ling_param[["ling.lln.alpha"]] <- 0.5
ling_param[["ling.lln.l50"]] <- 50
ling_param[["ling.bmt.alpha"]] <- 0.5
ling_param[["ling.bmt.l50"]] <- 50
ling_param[["ling.gil.alpha"]] <- 0.5
ling_param[["ling.gil.l50"]] <- 50
ling_param[["ling.init.sd"]] <- init.sigma$ms
ling_param[["ling_si_alpha1"]] <- 6.4e-9
ling_param[["ling_si_beta1"]] <- 1.4
ling_param[['ling_si_alpha2']] <- 1.7e-7
ling_param[["ling_si_beta2"]] <- 1.3
ling_param[['ling_si_alpha3']] <- 2.7e-5
ling_param[['ling_si_alpha4']] <- 4.2e-5
ling_param[['ling_si_alpha5']] <- 5.4e-5
ling_param[['ling_si_alpha6']] <- 5.8e-5
ling_param[['ling_si_alpha7']] <- 7.1e-5
ling_param[grepl('^lingimm\\.M\\.', names(ling_param))] <- 0.15
ling_param[grepl('^lingimm\\.init\\.', names(ling_param))] <- 1
ling_param[grepl('^lingmat\\.M\\.', names(ling_param))] <- 0.15
ling_param[grepl('^lingmat\\.init\\.', names(ling_param))] <- 1
ling_param[grepl('^ling\\.rec\\.', names(ling_param))] <- 1


#ling_model <- edit(ling_model)
result <- ling_model(ling_param)

tmb_param$value <- I(ling_param[rownames(tmb_param)])
tmb_param$lower <- vapply(tmb_param$value, function (x) 0.5 * x[[1]], numeric(1))
tmb_param$upper <- vapply(tmb_param$value, function (x) 2 * x[[1]], numeric(1))

tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$lower <- 0.0001
tmb_param[grepl('^ling\\.rec\\.', rownames(tmb_param)),]$upper <- 1e3
tmb_param[grepl('^lingmat\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
tmb_param[grepl('^lingmat\\.init\\.', rownames(tmb_param)),]$upper <- 1e3

tmb_param[grepl('^lingimm\\.init\\.', rownames(tmb_param)),]$lower <- 0.0001
tmb_param[grepl('^lingimm\\.init\\.', rownames(tmb_param)),]$upper <- 1e3


tmb_param[c('ling.init.sd',
            'lingimm.walpha',
            'lingimm.wbeta',
            'lingmat.walpha',
            'lingmat.wbeta',
            "ling_si_alpha1",
            "ling_si_beta1" ,
            'ling_si_alpha2',
            "ling_si_beta2" ,
            'ling_si_alpha3',
            'ling_si_alpha4',
            'ling_si_alpha5',
            'ling_si_alpha6',
            'ling_si_alpha7'),]$optimise <- FALSE
tmb_param[grepl('^lingimm\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^lingmat\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE


ling_model_tmb <- g3_tmb_adfun(tmb_ling,tmb_param)

ling_model_tmb$fn(g3_tmb_par(tmb_param))

#params <- 
#  tibble(switch = names(par), value = par, upper = 2*par, lower = 0.5*par, optimise = 0*par + 1, order = 1:length(par)) #%>% 
# Rgadget::init_guess('M[0-9+]$',optimise = 0) %>% 
# Rgadget::init_guess('__sd[0-9+]$',optimise = 0) %>% 
# Rgadget::init_guess('si_alpha[0-9+]$',optimise = 0) %>% 
# Rgadget::init_guess('si_beta[0-9]$',optimise = 0) %>% 
# arrange(order)
#%>% 
#  mutate(upper = ifelse(optimise == 0, 1.01*value,upper),
#         lower = ifelse(optimise == 0, 0.99*value,lower))

fit <- nlminb(g3_tmb_par(tmb_param), 
              ling_model_tmb$fn, ling_model_tmb$gr, 
              upper = g3_tmb_upper(tmb_param),
              lower = g3_tmb_lower(tmb_param),
              control = list(trace = TRUE, eval.max=2000, iter.max=1000))
#control = list(abs.tol = 1e-20,xf.tol = 0.1)) 
fit2 <- nlminb(fit$par, 
               ling_model_tmb$fn, ling_model_tmb$gr,
               upper = g3_tmb_upper(tmb_param),
               lower = g3_tmb_lower(tmb_param),
               control = list(trace = TRUE))
fit3 <- nlminb(fit3$par, 
               ling_model_tmb$fn, ling_model_tmb$gr,
               upper = g3_tmb_upper(tmb_param),
               lower = g3_tmb_lower(tmb_param),
               control = list(trace = TRUE, eval.max=2000, iter.max=1000))
