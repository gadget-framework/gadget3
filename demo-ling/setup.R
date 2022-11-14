library(mfdb)
library(tidyverse)
library(gadget3)

## Configure what parts of the demo run, set environment variables to turn on/off
## e.g. Sys.setenv(demo_ling_read_data = TRUE)
setdefault <- function (n, def) as.logical(Sys.getenv(n, as.character(def)))
demo_ling_read_data <- setdefault("demo_ling_read_data", FALSE)
demo_ling_dump_model <- Sys.getenv("demo_ling_dump_model", "")
demo_ling_run_r <- setdefault("demo_ling_run_r", TRUE)
demo_ling_compile_tmb <- setdefault("demo_ling_compile_tmb", TRUE)
demo_ling_run_tmb <- setdefault("demo_ling_run_tmb", TRUE)
demo_ling_optimize_tmb <- setdefault("demo_ling_optimize_tmb", TRUE)
demo_ling_fit <- setdefault("demo_ling_fit", FALSE)

# Create dump directory if we don't have one
if (nzchar(demo_ling_dump_model) && !dir.exists(demo_ling_dump_model)) dir.create(demo_ling_dump_model, recursive = TRUE)

## functions for inserting species name into param references
source("demo-ling/stock_param_functions.r")

## Some model parameters...
year_range <- 1982:lubridate::year(Sys.Date())

## Stock info.
species_name <- "ling"
species_code <- "LIN"

## -----------------------------------------------------------------------------

reitmapping <-
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

defaults <- list(
  area = mfdb::mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb::mfdb_timestep_quarterly,
  year = year_range,
  species = species_code)

# Map area names to integer area numbers (in this case only "1" ==> 1, but could be anything)
areas <- structure(
  seq_along(defaults$area),
  names = names(defaults$area))

#### Load required data objects ################################################

if(demo_ling_read_data){
  mdb <- mfdb('Iceland', db_params = list(host = 'mfdb.hafro.is'))
  #  mdb <- mfdb("../../mfdb/copy/iceland.duckdb")
  source('demo-ling/setup-fleet-data.R')
  source('demo-ling/setup-catchdistribution.R')
  source('demo-ling/setup-indices.R')
  source('demo-ling/setup-initial_parameters.R')
} else {
  fs::dir_ls('demo-ling/data') %>%
    stringr::str_subset('.Rdata') %>%
    lapply(load,.GlobalEnv)
}

##### Configure model actions #################################################

# Timekeeping for the model, i.e. how long we run for
time_actions <- list(
  g3a_time(start_year = min(defaults$year),
           end_year = max(defaults$year),
           defaults$timestep),
  list())

source('demo-ling/setup-model.R')  # Generates mat_actions / imm_actions
source('demo-ling/setup-fleets.R')  # Generates fleet_actions
source('demo-ling/setup-likelihood.R')  # Generates likelihood_actions

actions <- c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  time_actions)

# Generate reports for all abundance and catch ling_imm/ling_mat variables
actions <- c(actions, list(
    g3a_report_history(actions, '^ling_(imm|mat)__(num|wgt|igfs|lln|bmt|gil|foreign|suit_igfs|renewalnum|renewalwgt)$')))

##### Run r-based model #######################################################

# Turn actions into an R function
ling_model <- g3_to_r(actions)

# Get parameter template attached to function, fill it in
ling_param <- attr(ling_model, 'parameter_template')
ling_param[grepl('\\.walpha$', names(ling_param))] <- 2.27567436711055e-06
ling_param[grepl('\\.wbeta$', names(ling_param))] <- 3.20200445996187
ling_param[grepl('\\.bbin$', names(ling_param))] <- 6
ling_param[grepl('mat\\.M\\.', names(ling_param))] <- 0.15
ling_param[grepl('imm\\.M\\.', names(ling_param))] <- 0.15
ling_param[grepl('\\.rec\\.sd$', names(ling_param))] <- 1

ling_param[grepl('mat\\.init\\.sd', names(ling_param))] <-
  init.sigma %>% filter(age %in% 
                          g3_stock_def(ling_mat, 'minage'): 
                          g3_stock_def(ling_mat, 'maxage')) %>% .$ms

ling_param[grepl('imm\\.init\\.sd', names(ling_param))] <-
  init.sigma %>% filter(age %in% 
                          g3_stock_def(ling_imm, 'minage'): 
                          g3_stock_def(ling_imm, 'maxage')) %>% .$ms

## SI's
ling_param[grepl('si_beta1', names(ling_param))] <- 1
ling_param[grepl('si_beta2', names(ling_param))] <- 1
ling_param[grepl('si_igfs_si.+weight$',names(ling_param))] <- 1

## Old weights from gadget2
ling_param['cdist_sumofsquares_ldist_lln_weight'] <- 3331
ling_param['cdist_sumofsquares_aldist_lln_weight'] <- 2512
ling_param['cdist_sumofsquares_ldist_bmt_weight'] <- 1247
ling_param['cdist_sumofsquares_aldist_bmt_weight'] <- 1515
ling_param['cdist_sumofsquares_ldist_gil_weight'] <- 781
ling_param['cdist_sumofsquares_aldist_gil_weight'] <- 719
ling_param['cdist_sumofsquares_ldist_igfs_weight'] <- 6869
ling_param['cdist_sumofsquares_aldist_igfs_weight'] <- 11087
ling_param['cdist_sumofsquares_matp_igfs_weight'] <- 9
ling_param['adist_surveyindices_log_si_igfs_si1_weight'] <- 40
ling_param['adist_surveyindices_log_si_igfs_si2a_weight'] <- 8
ling_param['adist_surveyindices_log_si_igfs_si2b_weight'] <- 36
ling_param['adist_surveyindices_log_si_igfs_si3a_weight'] <- 19
ling_param['adist_surveyindices_log_si_igfs_si3b_weight'] <- 16
ling_param['adist_surveyindices_log_si_igfs_si3c_weight'] <- 13
ling_param['adist_surveyindices_log_si_igfs_si3d_weight'] <- 14.5

# You can edit the model code with:
#ling_model <- edit(ling_model)

if (nzchar(demo_ling_dump_model)) {
  writeLines(deparse(ling_model, width.cutoff = 500L), con = file.path(demo_ling_dump_model, 'demo-ling.compiled.R'))
}

if (demo_ling_run_r) {
    # Run model with params above
    result <- ling_model(ling_param)

    # nll
    print(result[[1]])

    # List all available reports
    print(names(attributes(result)))
}

##### Run TMB-based model #####################################################

# Turn actions into C++ objective function code
tmb_ling <- g3_to_tmb(actions)

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
tmb_param[c('ling_imm.walpha',
            'ling_imm.wbeta',
            'ling_mat.walpha',
            #"ling_si_alpha1",
            #"ling_si_beta1" ,
            #'ling_si_alpha2',
            #"ling_si_beta2" ,
            #'ling_si_alpha3',
            #'ling_si_alpha4',
            #'ling_si_alpha5',
            #'ling_si_alpha6',
            #'ling_si_alpha7',
            'ling_mat.wbeta'),]$optimise <- FALSE
tmb_param[grepl('^ling_imm\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^ling_mat\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^ling_imm\\.init\\.sd', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^ling_mat\\.init\\.sd', rownames(tmb_param)),]$optimise <- FALSE

if (nzchar(demo_ling_dump_model)) {
    writeLines(tmb_ling, con = file.path(demo_ling_dump_model, 'demo-ling.cpp'))
    capture.output(print(tmb_param), file = file.path(demo_ling_dump_model, 'demo-ling.tmbparam'))
}

if (demo_ling_compile_tmb) {
    # Compile and generate TMB ADFun (see ?TMB::MakeADFun)
    ling_model_tmb <- g3_tmb_adfun(tmb_ling,tmb_param)
    # writeLines(TMB::gdbsource(g3_tmb_adfun(tmb_ling, tmb_param, compile_flags = "-g", output_script = TRUE)))
}

if (demo_ling_run_tmb) {
    # Run model once, using g3_tmb_par to reshape tmb_param into param vector.
    # Will return nll
    ling_model_tmb$fn(g3_tmb_par(tmb_param))

    # Run model once, returning model report
    ling_model_tmb$report(g3_tmb_par(tmb_param))
}

if (demo_ling_optimize_tmb) {
    # Run model through R optimiser, using bounds set in tmb_param
    fit.opt <- optim(ling_model_tmb$par,
                     ling_model_tmb$fn,
                     ling_model_tmb$gr,
                     method = 'BFGS',
                     control = list(trace = 2,maxit = 1000, reltol = .Machine$double.eps^2))
    save(fit.opt, file="demo-ling/data/fit.opt.Rdata")
} else {
    load("demo-ling/data/fit.opt.Rdata")
}

if (demo_ling_run_tmb) {
    # Re-run with fitted parameters, returning report
    ling_model_tmb$report(fit.opt$par)
}

if (demo_ling_fit) {
    fit <- gadgetutils::g3_fit(ling_model, g3_tmb_relist(tmb_param, fit.opt$par))
    fit
}
