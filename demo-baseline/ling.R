structure(function (param) 
{
    inttypelookup_get <- function (lookup, key) 
    {
        out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
        if (length(out) < 1) {
            our_args <- as.list(sys.call())
            stop(key, " not in ", our_args[[2]])
        }
        return(out)
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
    renewal_dnorm <- array(dim = 35L, dimnames = NULL)
    ling_imm__midlen <- model_data$ling_imm__midlen
    ling_imm_stddev <- model_data$ling_imm_stddev
    ling_imm__num <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    ling_imm__area_idx <- (1)
    renewal_scaler <- 0
    ling_imm__wgt <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    ling_mat__minage <- 5L
    ling_mat__maxage <- 15L
    ling_mat__area <- 1L
    ling_mat__midlen <- model_data$ling_mat__midlen
    ling_mat_stddev <- model_data$ling_mat_stddev
    ling_mat__num <- array(dim = c(35L, 1L, 11L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15"
    )))
    ling_mat__area_idx <- (1)
    ling_mat__wgt <- array(dim = c(35L, 1L, 11L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15"
    )))
    igfs__catch <- array(dim = 1L, dimnames = list("area1"))
    ling_imm__totalpredate <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    ling_mat__totalpredate <- array(dim = c(35L, 1L, 11L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", 
    "age15")))
    ling_imm__igfs <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    igfs__area <- 1L
    igfs__area_idx <- (1)
    ling_mat__igfs <- array(dim = c(35L, 1L, 11L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15"
    )))
    predate_totalfleet_E <- 0
    inttypelookup_zip <- function (keys, values) 
    {
        list(keys = keys, values = values)
    }
    igfs_totaldata__keys <- model_data$igfs_totaldata__keys
    igfs_totaldata__values <- model_data$igfs_totaldata__values
    igfs_totaldata__lookup <- inttypelookup_zip(igfs_totaldata__keys, igfs_totaldata__values)
    ling_imm__overconsumption <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    ling_mat__overconsumption <- array(dim = c(35L, 1L, 11L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", 
    "age15")))
    ling_imm__transitioning_num <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    ling_imm__transitioning_wgt <- array(dim = c(35L, 1L, 8L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), "area1", c("age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    ling_imm__growth_l <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_imm__dl <- model_data$ling_imm__dl
    ling_imm__growth_w <- array(dim = 35L, dimnames = NULL)
    ling_mat__growth_l <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_mat__dl <- model_data$ling_mat__dl
    ling_mat__growth_w <- array(dim = 35L, dimnames = NULL)
    cdist_ldist_lln_obs__num <- array(dim = c(35L, 100L), dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), c("1994.1", "1994.2", "1994.3", "1994.4", "1995.1", "1995.2", "1995.3", "1995.4", "1996.1", "1996.2", 
    "1996.3", "1996.4", "1997.1", "1997.2", "1997.3", "1997.4", "1998.1", "1998.2", "1998.3", "1998.4", "1999.1", "1999.2", "1999.3", "1999.4", "2000.1", "2000.2", "2000.3", "2000.4", "2001.1", "2001.2", "2001.3", "2001.4", "2002.1", "2002.2", "2002.3", "2002.4", "2003.1", "2003.2", "2003.3", "2003.4", "2004.1", "2004.2", "2004.3", "2004.4", "2005.1", "2005.2", "2005.3", "2005.4", "2006.1", "2006.2", "2006.3", "2006.4", "2007.1", "2007.2", "2007.3", "2007.4", "2008.1", "2008.2", "2008.3", "2008.4", 
    "2009.1", "2009.2", "2009.3", "2009.4", "2010.1", "2010.2", "2010.3", "2010.4", "2011.1", "2011.2", "2011.3", "2011.4", "2012.1", "2012.2", "2012.3", "2012.4", "2013.1", "2013.2", "2013.3", "2013.4", "2014.1", "2014.2", "2014.3", "2014.4", "2015.1", "2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3", "2016.4", "2017.1", "2017.2", "2017.3", "2017.4", "2018.1", "2018.2", "2018.3", "2018.4")))
    ldist_lln_number <- model_data$ldist_lln_number
    cdist_ldist_lln_model__num <- array(dim = 35L, dimnames = list(c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156")))
    cdist_ldist_lln_obs__totalsteps <- 4L
    cdist_ldist_lln_obs__steplookup <- model_data$cdist_ldist_lln_obs__steplookup
    while (TRUE) {
        {
            comment("g3a_time")
            cur_time <- cur_time + 1
            if (cur_time > total_steps) 
                return(nll)
            cur_year <- start_year + (cur_time%/%step_count)
            cur_step <- (cur_time%%step_count) + 1
            cur_step_len <- step_lengths[[(cur_step)]]
            cur_step_final <- cur_step == step_count
            cat(sprintf("** Tick: %d-%d\n", cur_year, cur_step))
        }
        {
            comment("g3a_renewal_normalparam for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (cur_time == 0L) {
                    renewal_dnorm <- (ling_imm__midlen - param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))) * (1/ling_imm_stddev[[age - 3 + 1]])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(renewal_dnorm^2) * 0.5)
                    renewal_scaler <- 10000/sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * renewal_scaler * (param[["lingimm.init.scalar"]] * exp(-1 * (param[["lingimm.M"]] + param[["ling.init.F"]]) * age) * param[["lingimm.init"]][[age - 3 + 1]])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                  }
                }
            }
        }
        {
            comment("g3a_renewal_normalparam for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                {
                  area <- ling_mat__area
                  if (cur_time == 0L) {
                    renewal_dnorm <- (ling_mat__midlen - param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))) * (1/ling_mat_stddev[[age - 5 + 1]])
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- exp(-(renewal_dnorm^2) * 0.5)
                    renewal_scaler <- 10000/sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * renewal_scaler * (param[["lingmat.init.scalar"]] * exp(-1 * (param[["lingmat.M"]] + param[["ling.init.F"]]) * age) * param[["lingmat.init"]][[age - 5 + 1]])
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- param[["lingmat.walpha"]] * ling_mat__midlen^param[["lingmat.wbeta"]]
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
                      ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] <- (1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_imm__midlen - param[["ling.igfs.l50"]]))) * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
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
                      ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] <- (1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_mat__midlen - param[["ling.igfs.l50"]]))) * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx])
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
                      predate_totalfleet_E <- (inttypelookup_get(igfs_totaldata__lookup, area * 1000000L + cur_year * 100L + cur_step))
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
                      predate_totalfleet_E <- (inttypelookup_get(igfs_totaldata__lookup, area * 1000000L + cur_year * 100L + cur_step))
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
                    ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx] <- pmin((ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * 0.95)/pmax(ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx], 1e-05), 1)
                    ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx]
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] - (ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx]/pmax(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx], 1e-05))
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
                    ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx] <- pmin((ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * 0.95)/pmax(ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx], 1e-05), 1)
                    ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx]
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] - (ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx]/pmax(ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx], 1e-05))
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
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * exp(-param[["lingimm.M"]] * cur_step_len)
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
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * exp(-param[["lingmat.M"]] * cur_step_len)
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
                    ling_imm__growth_l <- growth_bbinom((param[["ling.Linf"]] - ling_imm__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)), ling_imm__dl, length(ling_imm__dl), param[["ling.bbin"]] * 10)
                    ling_imm__growth_w <- param[["lingimm.walpha"]] * ((ling_imm__midlen + (param[["ling.Linf"]] - ling_imm__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)))^param[["lingimm.wbeta"]] - ling_imm__midlen^param[["lingimm.wbeta"]])
                    {
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] - (ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (1/(1 + exp(0))))
                    }
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- g3a_grow_apply(ling_imm__growth_l, ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- (ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__growth_w)/pmax(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], 1e-05)
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
                    ling_mat__growth_l <- growth_bbinom((param[["ling.Linf"]] - ling_mat__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)), ling_mat__dl, length(ling_mat__dl), param[["ling.bbin"]] * 10)
                    ling_mat__growth_w <- param[["lingmat.walpha"]] * ((ling_mat__midlen + (param[["ling.Linf"]] - ling_mat__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)))^param[["lingmat.wbeta"]] - ling_mat__midlen^param[["lingmat.wbeta"]])
                    {
                    }
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- g3a_grow_apply(ling_mat__growth_l, ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] + ling_mat__growth_w)/pmax(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 1e-05)
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
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + (ling_imm__transitioning_wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1)
                          ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + (ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1)
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/pmax(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 1e-05)
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
                    renewal_dnorm <- (ling_imm__midlen - param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))) * (1/ling_imm_stddev[[age - 3 + 1]])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(renewal_dnorm^2) * 0.5)
                    renewal_scaler <- 10000/sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * renewal_scaler * (param[["ling.rec.scalar"]] * param[["ling.rec"]][[cur_year - start_year + 1]])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
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
                    cdist_ldist_lln_model__num[] <- cdist_ldist_lln_model__num[] + ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]/pmax(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx], 1e-05)
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
                    cdist_ldist_lln_model__num[] <- cdist_ldist_lln_model__num[] + ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]/pmax(ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx], 1e-05)
                  }
                }
            }
        }
        {
            if (TRUE) {
                comment("Compare cdist_ldist_lln_model to cdist_ldist_lln_obs")
                {
                  cdist_ldist_lln_obs__time_idx <- ((cur_year - 1994L) * cdist_ldist_lln_obs__totalsteps) + cdist_ldist_lln_obs__steplookup[[(cur_step)]]
                  if (cdist_ldist_lln_obs__time_idx >= (1) && cdist_ldist_lln_obs__time_idx <= (100)) {
                    nll <- nll + 1 * sum((cdist_ldist_lln_model__num[]/max(sum(cdist_ldist_lln_model__num[]), 1e-05) - cdist_ldist_lln_obs__num[, cdist_ldist_lln_obs__time_idx]/max(sum(cdist_ldist_lln_obs__num[, cdist_ldist_lln_obs__time_idx]), 1e-05))^2)
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
                    ling_imm__wgt[, , ling_imm__age_idx + 1] <- ling_imm__wgt[, , ling_imm__age_idx + 1]/pmax(ling_imm__num[, , ling_imm__age_idx + 1], 1e-05)
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
                    ling_mat__wgt[, , ling_mat__age_idx + 1] <- ling_mat__wgt[, , ling_mat__age_idx + 1]/pmax(ling_mat__num[, , ling_mat__age_idx + 1], 1e-05)
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
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + (ling_imm__transitioning_wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1)
                          ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + (ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] * 1)
                          ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/pmax(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 1e-05)
                        }
                      }
                    }
                  }
                }
            }
        }
    }
    stop("Should have return()ed somewhere in the loop")
}, class = c("g3_r", "function"))
