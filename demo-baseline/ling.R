structure(function (param) 
{
    Rprintf <- function (...) 
    {
        cat(sprintf(...))
    }
    inttypelookup_getdefault <- function (lookup, key, def) 
    {
        out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
        return(if (length(out) < 1) def else out)
    }
    logspace_add_vec <- function (a, b) 
    {
        pmax(a, b) + log1p(exp(pmin(a, b) - pmax(a, b)))
    }
    growth_bbinom <- function (dmu, lengthgrouplen, binn, beta) 
    {
        delt_l <- dmu/lengthgrouplen
        alpha <- (beta * delt_l)/(binn - delt_l)
        x <- 0:binn
        na <- length(alpha)
        n <- length(x) - 1
        alpha <- rep(alpha, n + 1)
        x <- rep(x, each = na)
        val <- exp(lgamma(n + 1) + lgamma(alpha + beta) + lgamma(n - x + beta) + lgamma(x + alpha) - lgamma(n - x + 1) - lgamma(x + 1) - lgamma(n + alpha + beta) - lgamma(beta) - lgamma(alpha))
        dim(val) <- c(na, n + 1)
        return(val)
    }
    g3a_grow_apply <- function (lg_deltas, input_num) 
    {
        na <- dim(lg_deltas)[[1]]
        n <- dim(lg_deltas)[[2]] - 1
        growth.matrix <- array(0, c(na, na))
        for (lg in 1:na) {
            if (lg == na) {
                growth.matrix[na, na] <- 1
            }
            else if (lg + n > na) {
                growth.matrix[lg, lg:(na - 1)] <- lg_deltas[lg, 1:(na - lg)]
                growth.matrix[lg, na] <- sum(lg_deltas[lg, (na - lg + 1):(n + 1)])
            }
            else {
                growth.matrix[lg, lg:(n + lg)] <- lg_deltas[lg, ]
            }
        }
        return(Matrix::colSums(growth.matrix * as.vector(input_num)))
    }
    intintlookup_getdefault <- function (lookup, key, def) 
    {
        out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
        return(if (length(out) < 1) def else out)
    }
    logspace_add <- function (a, b) 
    {
        pmax(a, b) + log1p(exp(pmin(a, b) - pmax(a, b)))
    }
    cur_time <- -1L
    step_lengths <- model_data$step_lengths
    end_year <- 2018L
    start_year <- 1994L
    total_steps <- length(step_lengths) * (end_year - start_year) + length(step_lengths) - 1
    nll <- 0
    cur_year <- 0L
    step_count <- 4L
    cur_step <- 0L
    cur_step_len <- 0L
    cur_step_final <- FALSE
    ling_imm__minage <- 3L
    ling_imm__maxage <- 10L
    ling_imm__area <- 1L
    ling_imm__num <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    ling_imm__area_idx <- (1)
    ling_imm__midlen <- model_data$ling_imm__midlen
    ling_imm_stddev <- model_data$ling_imm_stddev
    ling_imm__wgt <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    ling_mat__minage <- 5L
    ling_mat__maxage <- 15L
    ling_mat__area <- 1L
    ling_mat__num <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", "age10", 
    "age11", "age12", "age13", "age14", "age15")))
    ling_mat__area_idx <- (1)
    ling_mat__midlen <- model_data$ling_mat__midlen
    ling_mat_stddev <- model_data$ling_mat_stddev
    ling_mat__wgt <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", "age10", 
    "age11", "age12", "age13", "age14", "age15")))
    igfs__catch <- array(dim = c(area = 1L), dimnames = list(area = "area1"))
    ling_imm__totalpredate <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_mat__totalpredate <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", 
    "age10", "age11", "age12", "age13", "age14", "age15")))
    ling_imm__igfs <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    igfs__area <- 1L
    igfs__area_idx <- (1)
    ling_mat__igfs <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", "age10", 
    "age11", "age12", "age13", "age14", "age15")))
    predate_totalfleet_E <- 0
    inttypelookup_zip <- function (keys, values) 
    {
        list(keys = keys, values = values)
    }
    igfs_totaldata__keys <- model_data$igfs_totaldata__keys
    igfs_totaldata__values <- model_data$igfs_totaldata__values
    igfs_totaldata__lookup <- inttypelookup_zip(igfs_totaldata__keys, igfs_totaldata__values)
    ling_imm__overconsumption <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_mat__overconsumption <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", 
    "age10", "age11", "age12", "age13", "age14", "age15")))
    ling_imm__transitioning_num <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_imm__transitioning_wgt <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_imm__growth_l <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_imm__dl <- model_data$ling_imm__dl
    ling_imm__growth_w <- array(dim = 35L, dimnames = NULL)
    ling_mat__growth_l <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_mat__dl <- model_data$ling_mat__dl
    ling_mat__growth_w <- array(dim = 35L, dimnames = NULL)
    ling_imm__renewalnum <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    ling_imm__renewalwgt <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    cdist_ldist_lln_obs__num <- array(dim = c(length = 35L, time = 92L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), time = c("1994004", "1995001", "1995003", "1995004", "1996001", "1996003", 
    "1996004", "1997001", "1997002", "1997003", "1997004", "1998001", "1998002", "1998003", "1998004", "1999001", "1999002", "1999003", "1999004", "2000001", "2000002", "2000003", "2000004", "2001001", "2001003", "2001004", "2002001", "2002002", "2002003", "2002004", "2003001", "2003002", "2003003", "2003004", "2004001", "2004002", "2004003", "2004004", "2005001", "2005002", "2005003", "2005004", "2006001", "2006002", "2006003", "2006004", "2007001", "2007002", "2007003", "2007004", "2008001", "2008002", 
    "2008003", "2008004", "2009001", "2009002", "2009003", "2009004", "2010001", "2010002", "2010003", "2010004", "2011001", "2011002", "2011003", "2011004", "2012001", "2012002", "2012003", "2012004", "2013001", "2013002", "2013003", "2013004", "2014001", "2014002", "2014003", "2014004", "2015001", "2015002", "2015003", "2015004", "2016001", "2016002", "2016003", "2016004", "2017002", "2017003", "2017004", "2018001", "2018002", "2018003")))
    ldist_lln_number <- model_data$ldist_lln_number
    cdist_ldist_lln_model__num <- array(dim = c(length = 35L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156")))
    intintlookup_zip <- function (keys, values) 
    {
        list(keys = keys, values = values)
    }
    times_cdist_ldist_lln_obs__keys <- model_data$times_cdist_ldist_lln_obs__keys
    times_cdist_ldist_lln_obs__values <- model_data$times_cdist_ldist_lln_obs__values
    times_cdist_ldist_lln_obs__lookup <- intintlookup_zip(times_cdist_ldist_lln_obs__keys, times_cdist_ldist_lln_obs__values)
    while (TRUE) {
        {
            comment("g3a_time")
            cur_time <- cur_time + 1
            if (cur_time > total_steps) 
                return(nll)
            cur_year <- start_year + (cur_time%/%step_count)
            cur_step <- (cur_time%%step_count) + 1
            cur_step_len <- step_lengths[[cur_step]]
            cur_step_final <- cur_step == step_count
            Rprintf("** Tick: %d-%d\n", cur_year, cur_step)
        }
        {
            comment("g3a_initialconditions_normalparam for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (cur_time == 0L) {
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(((ling_imm__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]]))))))) * (1/(ling_imm_stddev[[age - 3 + 1]])))^2) * 0.5)
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (10000/sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]))
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (param[["lingimm.init.scalar"]] * exp(-1 * (param[["lingimm.M"]] + param[["ling.init.F"]]) * age) * param[["lingimm.init"]][[age - 3 + 1]])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- (param[["lingimm.walpha"]]) * ling_imm__midlen^(param[["lingimm.wbeta"]])
                  }
                }
            }
        }
        {
            comment("g3a_initialconditions_normalparam for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  if (cur_time == 0L) {
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- exp(-(((ling_mat__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]]))))))) * (1/(ling_mat_stddev[[age - 5 + 1]])))^2) * 0.5)
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * (10000/sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]))
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * (param[["lingmat.init.scalar"]] * exp(-1 * (param[["lingmat.M"]] + param[["ling.init.F"]]) * age) * param[["lingmat.init"]][[age - 5 + 1]])
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (param[["lingmat.walpha"]]) * ling_mat__midlen^(param[["lingmat.wbeta"]])
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for igfs")
            igfs__catch[] <- 0
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            ling_imm__totalpredate[] <- 0
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            ling_mat__totalpredate[] <- 0
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            comment("Zero counter of biomass caught for this fleet")
            ling_imm__igfs[] <- 0
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    {
                      comment("Collect all suitable biomass for fleet")
                      ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] <- ((1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_imm__midlen - param[["ling.igfs.l50"]])))) * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]))
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            comment("Zero counter of biomass caught for this fleet")
            ling_mat__igfs[] <- 0
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  if (area == igfs__area) {
                    {
                      comment("Collect all suitable biomass for fleet")
                      ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] <- ((1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_mat__midlen - param[["ling.igfs.l50"]])))) * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx])
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]))
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    {
                      comment("Scale fleet amount by total expected catch")
                      predate_totalfleet_E <- (inttypelookup_getdefault(igfs_totaldata__lookup, area * 1000000L + cur_year * 100L + cur_step, 0))
                      ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] <- predate_totalfleet_E * ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]/igfs__catch[igfs__area_idx]
                      ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  if (area == igfs__area) {
                    {
                      comment("Scale fleet amount by total expected catch")
                      predate_totalfleet_E <- (inttypelookup_getdefault(igfs_totaldata__lookup, area * 1000000L + cur_year * 100L + cur_step, 0))
                      ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] <- predate_totalfleet_E * ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]/igfs__catch[igfs__area_idx]
                      ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] + ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]
                    }
                  }
                }
            }
        }
        {
            comment("Zero fleet catch before working out post-adjustment value")
            igfs__catch[] <- 0
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  {
                    comment("Prey overconsumption coefficient")
                    ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx] <- logspace_add_vec(-200 * (ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * 0.95)/logspace_add_vec(ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx], 0), -200)/-200
                    ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx]
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] - (ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx]/logspace_add_vec(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx], 0))
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  {
                    comment("Prey overconsumption coefficient")
                    ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx] <- logspace_add_vec(-200 * (ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * 0.95)/logspace_add_vec(ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx], 0), -200)/-200
                    ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx]
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] - (ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx]/logspace_add_vec(ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx], 0))
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    {
                      comment("Scale caught amount by overconsumption, update variables")
                      ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx]
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]))
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  if (area == igfs__area) {
                    {
                      comment("Scale caught amount by overconsumption, update variables")
                      ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx]
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]))
                    }
                  }
                }
            }
        }
        if (TRUE) {
            comment("Natural mortality for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  {
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (exp(-(param[["lingimm.M"]]) * cur_step_len))
                  }
                }
            }
        }
        if (TRUE) {
            comment("Natural mortality for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  {
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * (exp(-(param[["lingmat.M"]]) * cur_step_len))
                  }
                }
            }
        }
        {
            comment("g3a_grow for ling_imm")
            if (cur_step_final) {
                comment("Reset transitioning arrays")
                ling_imm__transitioning_num[] <- 0
                ling_imm__transitioning_wgt[] <- ling_imm__wgt[]
            }
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (TRUE) {
                    comment("Calculate increase in length/weight for each lengthgroup")
                    ling_imm__growth_l <- growth_bbinom(((param[["ling.Linf"]]) - ling_imm__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)), ling_imm__dl, length(ling_imm__dl), param[["ling.bbin"]] * 10)
                    ling_imm__growth_w <- (param[["lingimm.walpha"]]) * ((ling_imm__midlen + (((param[["ling.Linf"]]) - ling_imm__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len))))^(param[["lingimm.wbeta"]]) - ling_imm__midlen^(param[["lingimm.wbeta"]]))
                    {
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] - (ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (1/(1 + exp(0 - (0.001 * param[["ling.mat1"]]) * (ling_imm__midlen - (param[["ling.mat2"]]))))))
                    }
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]
                    if (FALSE) 
                      ling_imm__prevtotal <- sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- g3a_grow_apply(ling_imm__growth_l, ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    if (FALSE) 
                      stopifnot(ling_imm__prevtotal - sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]) < 1e-04)
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- (ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__growth_w)/logspace_add_vec(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], 0)
                  }
                }
            }
        }
        {
            comment("g3a_grow for ling_mat")
            if (cur_step_final) {
            }
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  if (TRUE) {
                    comment("Calculate increase in length/weight for each lengthgroup")
                    ling_mat__growth_l <- growth_bbinom(((param[["ling.Linf"]]) - ling_mat__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)), ling_mat__dl, length(ling_mat__dl), param[["ling.bbin"]] * 10)
                    ling_mat__growth_w <- (param[["lingmat.walpha"]]) * ((ling_mat__midlen + (((param[["ling.Linf"]]) - ling_mat__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len))))^(param[["lingmat.wbeta"]]) - ling_mat__midlen^(param[["lingmat.wbeta"]]))
                    {
                    }
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]
                    if (FALSE) 
                      ling_mat__prevtotal <- sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- g3a_grow_apply(ling_mat__growth_l, ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    if (FALSE) 
                      stopifnot(ling_mat__prevtotal - sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) < 1e-04)
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] + ling_mat__growth_w)/logspace_add_vec(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 0)
                  }
                }
            }
        }
        {
            {
                comment("Move ling_imm to ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1
                  {
                    area <- ling_mat__area
                    if (age >= ling_imm__minage && age <= ling_imm__maxage) {
                      ling_imm__age_idx <- age - ling_imm__minage + 1
                      if (area == ling_imm__area) {
                        if (cur_step_final) {
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + ling_imm__transitioning_wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1
                          ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/logspace_add_vec(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 0)
                        }
                      }
                    }
                  }
                }
            }
        }
        {
            comment("g3a_renewal_normalparam for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (cur_step == 1 && age == 3) {
                    ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(((ling_imm__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]]))))))) * (1/(ling_imm_stddev[[age - 3 + 1]])))^2) * 0.5)
                    ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * (10000/sum(ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx]))
                    ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * (param[["ling.rec.scalar"]] * param[["ling.rec"]][[cur_year - start_year + 1]])
                    ling_imm__renewalwgt[, ling_imm__area_idx, ling_imm__age_idx] <- (param[["lingimm.walpha"]]) * ling_imm__midlen^(param[["lingimm.wbeta"]])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx]
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] + (ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__renewalwgt[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx]/logspace_add_vec(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], 0)
                  }
                }
            }
        }
        {
            comment("Initial data / reset observations for ldist_lln")
            if (cur_time == 0) {
                cdist_ldist_lln_obs__num[] <- ldist_lln_number
                cdist_ldist_lln_model__num[] <- 0
            }
        }
        {
            comment("Collect catch from igfs/ling_imm for cdist_ldist_lln_model")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  {
                    cdist_ldist_lln_model__num[] <- cdist_ldist_lln_model__num[] + ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]/logspace_add_vec(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx], 0)
                  }
                }
            }
        }
        {
            comment("Collect catch from igfs/ling_mat for cdist_ldist_lln_model")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  {
                    cdist_ldist_lln_model__num[] <- cdist_ldist_lln_model__num[] + ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]/logspace_add_vec(ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx], 0)
                  }
                }
            }
        }
        {
            if (TRUE) {
                comment("Compare cdist_ldist_lln_model to cdist_ldist_lln_obs")
                {
                  cdist_ldist_lln_obs__time_idx <- intintlookup_getdefault(times_cdist_ldist_lln_obs__lookup, cur_year * 1000 + cur_step, -1)
                  if (cdist_ldist_lln_obs__time_idx >= (1)) {
                    nll <- nll + 1 * sum((cdist_ldist_lln_model__num[]/logspace_add(sum(cdist_ldist_lln_model__num[]), 0) - cdist_ldist_lln_obs__num[, cdist_ldist_lln_obs__time_idx]/logspace_add(sum(cdist_ldist_lln_obs__num[, cdist_ldist_lln_obs__time_idx]), 0))^2)
                  }
                }
                cdist_ldist_lln_model__num[] <- 0
            }
        }
        if (cur_step_final) {
            comment("g3a_age for ling_imm")
            for (age in seq(ling_imm__maxage, ling_imm__minage, by = -1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  if (age == ling_imm__maxage) {
                    {
                      comment("Move oldest ling_imm")
                      ling_imm__transitioning_num[] <- 0
                      ling_imm__transitioning_wgt[] <- ling_imm__wgt[]
                      ling_imm__transitioning_num[, , ling_imm__age_idx] <- ling_imm__num[, , ling_imm__age_idx]
                      ling_imm__num[, , ling_imm__age_idx] <- 0
                    }
                  }
                  else {
                    ling_imm__wgt[, , ling_imm__age_idx + 1] <- ling_imm__wgt[, , ling_imm__age_idx + 1] * ling_imm__num[, , ling_imm__age_idx + 1]
                    ling_imm__num[, , ling_imm__age_idx + 1] <- ling_imm__num[, , ling_imm__age_idx + 1] + ling_imm__num[, , ling_imm__age_idx]
                    ling_imm__wgt[, , ling_imm__age_idx + 1] <- ling_imm__wgt[, , ling_imm__age_idx + 1] + (ling_imm__wgt[, , ling_imm__age_idx] * ling_imm__num[, , ling_imm__age_idx])
                    ling_imm__wgt[, , ling_imm__age_idx + 1] <- ling_imm__wgt[, , ling_imm__age_idx + 1]/logspace_add_vec(ling_imm__num[, , ling_imm__age_idx + 1], 0)
                    ling_imm__num[, , ling_imm__age_idx] <- 0
                    ling_imm__wgt[, , ling_imm__age_idx] <- 1e-05
                  }
                }
            }
        }
        if (cur_step_final) {
            comment("g3a_age for ling_mat")
            for (age in seq(ling_mat__maxage, ling_mat__minage, by = -1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  if (age == ling_mat__maxage) {
                    comment("Oldest ling_mat is a plus-group")
                  }
                  else {
                    ling_mat__wgt[, , ling_mat__age_idx + 1] <- ling_mat__wgt[, , ling_mat__age_idx + 1] * ling_mat__num[, , ling_mat__age_idx + 1]
                    ling_mat__num[, , ling_mat__age_idx + 1] <- ling_mat__num[, , ling_mat__age_idx + 1] + ling_mat__num[, , ling_mat__age_idx]
                    ling_mat__wgt[, , ling_mat__age_idx + 1] <- ling_mat__wgt[, , ling_mat__age_idx + 1] + (ling_mat__wgt[, , ling_mat__age_idx] * ling_mat__num[, , ling_mat__age_idx])
                    ling_mat__wgt[, , ling_mat__age_idx + 1] <- ling_mat__wgt[, , ling_mat__age_idx + 1]/logspace_add_vec(ling_mat__num[, , ling_mat__age_idx + 1], 0)
                    ling_mat__num[, , ling_mat__age_idx] <- 0
                    ling_mat__wgt[, , ling_mat__age_idx] <- 1e-05
                  }
                }
            }
        }
        {
            {
                comment("Move ling_imm to ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1
                  {
                    area <- ling_mat__area
                    if (age >= ling_imm__minage && age <= ling_imm__maxage) {
                      ling_imm__age_idx <- age - ling_imm__minage + 1
                      if (area == ling_imm__area) {
                        if (cur_step_final) {
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + ling_imm__transitioning_wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1
                          ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/logspace_add_vec(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 0)
                        }
                      }
                    }
                  }
                }
            }
        }
    }
    stop("Should have return()ed somewhere in the loop")
}, class = c("g3_r", "function"), parameter_template = list(ling.Linf = NA, ling.k = NA, ling.recl = NA, lingimm.init.scalar = NA, lingimm.M = NA, ling.init.F = NA, lingimm.init = NA, lingimm.walpha = NA, lingimm.wbeta = NA, lingmat.init.scalar = NA, lingmat.M = NA, lingmat.init = NA, lingmat.walpha = NA, lingmat.wbeta = NA, ling.igfs.alpha = NA, ling.igfs.l50 = NA, ling.bbin = NA, ling.mat1 = NA, ling.mat2 = NA, ling.rec.scalar = NA, ling.rec = NA))
