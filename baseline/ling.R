structure(function (param) 
{
    debugf <- function (...) 
    {
        cat(sprintf(...))
    }
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
    steps <- model_data$steps
    end_year <- 1985L
    start_year <- 1983L
    total_steps <- length(steps) * (end_year - start_year) + length(steps) - 1
    nll <- 0
    cur_year <- 0L
    step_count <- 4L
    cur_step <- 0L
    cur_step_len <- 0L
    cur_step_final <- FALSE
    ling_imm__minage <- 3L
    ling_imm__maxage <- 10L
    ling_imm__area <- 1L
    renewal_dnorm <- array(dim = 35L)
    ling_imm__midlen <- model_data$ling_imm__midlen
    ling_imm_stddev <- model_data$ling_imm_stddev
    ling_imm__num <- array(dim = c(35L, 1L, 8L))
    ling_imm__area_idx <- (1)
    renewal_scaler <- 0
    ling_imm__wgt <- array(dim = c(35L, 1L, 8L))
    ling_mat__minage <- 5L
    ling_mat__maxage <- 15L
    ling_mat__areas <- model_data$ling_mat__areas
    ling_mat__midlen <- model_data$ling_mat__midlen
    ling_mat_stddev <- model_data$ling_mat_stddev
    ling_mat__num <- array(dim = c(35L, 2L, 11L))
    ling_mat__wgt <- array(dim = c(35L, 2L, 11L))
    igfs__catch <- array(dim = 1L)
    ling_imm__totalpredate <- array(dim = c(35L, 1L, 8L))
    ling_mat__totalpredate <- array(dim = c(35L, 2L, 11L))
    igfs__ling_imm <- array(dim = c(35L, 1L, 8L))
    igfs__area <- 1L
    igfs__area_idx <- (1)
    igfs__ling_mat <- array(dim = c(35L, 2L, 11L))
    predate_totalfleet_E <- 0
    inttypelookup_zip <- function (keys, values) 
    {
        list(keys = keys, values = values)
    }
    igfs_totaldata__keys <- model_data$igfs_totaldata__keys
    igfs_totaldata__values <- model_data$igfs_totaldata__values
    igfs_totaldata__lookup <- inttypelookup_zip(igfs_totaldata__keys, igfs_totaldata__values)
    ling_imm__overconsumption <- array(dim = c(35L, 1L, 8L))
    ling_mat__overconsumption <- array(dim = c(35L, 2L, 11L))
    ling_imm__growth_l <- array(dim = c(0L, 0L))
    ling_imm__dl <- model_data$ling_imm__dl
    ling_imm__growth_w <- array(dim = 35L)
    ling_mat__growth_l <- array(dim = c(0L, 0L))
    ling_mat__dl <- model_data$ling_mat__dl
    ling_mat__growth_w <- array(dim = 35L)
    matured_ling_imm__wgt <- array(dim = c(35L, 1L, 8L))
    matured_ling_imm__minage <- 3L
    matured_ling_imm__maxage <- 10L
    matured_ling_imm__area <- 1L
    matured_ling_imm__num <- array(dim = c(35L, 1L, 8L))
    matured_ling_imm__area_idx <- (1)
    while (TRUE) {
        {
            comment("g3a_time")
            cur_time <- cur_time + 1
            if (cur_time > total_steps) 
                return(nll)
            cur_year <- start_year + (cur_time%/%step_count)
            cur_step <- (cur_time%%step_count) + 1
            cur_step_len <- steps[[(cur_step)]]
            cur_step_final <- cur_step == step_count
            debugf("** Tick: %d-%d\n", cur_year, cur_step)
        }
        {
            comment("g3a_renewal_normalparam for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (cur_time == 0L) {
                    renewal_dnorm <- (ling_imm__midlen - param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))) * (1/ling_imm_stddev[[ling_imm__age_idx]])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(renewal_dnorm^2) * 0.5)
                    renewal_scaler <- 10000/sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * renewal_scaler * (param[["lingimm.init.scalar"]] * exp(-1 * (param[["lingimm.M"]] + param[["ling.init.F"]]) * age) * param[[paste0("lingimm.init.", age)]])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                  }
                }
            }
        }
        {
            comment("g3a_renewal_normalparam for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  if (cur_time == 0L) {
                    renewal_dnorm <- (ling_mat__midlen - param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))) * (1/ling_mat_stddev[[ling_mat__age_idx]])
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- exp(-(renewal_dnorm^2) * 0.5)
                    renewal_scaler <- 10000/sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * renewal_scaler * (param[["lingmat.init.scalar"]] * exp(-1 * (param[["lingmat.M"]] + param[["ling.init.F"]]) * age) * param[[paste0("lingmat.init.", age)]])
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
            igfs__ling_imm[] <- 0
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    {
                      comment("Collect all suitable biomass for fleet")
                      igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx] <- (1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_imm__midlen - param[["ling.igfs.l50"]]))) * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx]))
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            comment("Zero counter of biomass caught for this fleet")
            igfs__ling_mat[] <- 0
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  if (area == igfs__area) {
                    {
                      comment("Collect all suitable biomass for fleet")
                      igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx] <- (1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_mat__midlen - param[["ling.igfs.l50"]]))) * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx])
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx]))
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    {
                      comment("Scale fleet amount by total expected catch")
                      predate_totalfleet_E <- (inttypelookup_get(igfs_totaldata__lookup, area * 1000000L + cur_year * 100L + cur_step))
                      igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx] <- predate_totalfleet_E * igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx]/igfs__catch[igfs__area_idx]
                      ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] + igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx]
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  if (area == igfs__area) {
                    {
                      comment("Scale fleet amount by total expected catch")
                      predate_totalfleet_E <- (inttypelookup_get(igfs_totaldata__lookup, area * 1000000L + cur_year * 100L + cur_step))
                      igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx] <- predate_totalfleet_E * igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx]/igfs__catch[igfs__area_idx]
                      ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] + igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx]
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
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  {
                    comment("Prey overconsumption coefficient")
                    ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx] <- pmin((ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * 0.95)/ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx], 1)
                    ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx]
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  {
                    comment("Prey overconsumption coefficient")
                    ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx] <- pmin((ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * 0.95)/ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx], 1)
                    ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx]
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    {
                      comment("Scale caught amount by overconsumption, update variables")
                      igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx] <- igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__overconsumption[, ling_imm__area_idx, ling_imm__age_idx]
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(igfs__ling_imm[, ling_imm__area_idx, ling_imm__age_idx]))
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] - (ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx]/ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                    }
                  }
                }
            }
        }
        {
            comment("g3a_predate_totalfleet for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  if (area == igfs__area) {
                    {
                      comment("Scale caught amount by overconsumption, update variables")
                      igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx] <- igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__overconsumption[, ling_mat__area_idx, ling_mat__age_idx]
                      igfs__catch[igfs__area_idx] <- (igfs__catch[igfs__area_idx] + sum(igfs__ling_mat[, ling_mat__area_idx, ling_mat__age_idx]))
                      ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] - (ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx]/ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx])
                    }
                  }
                }
            }
        }
        {
            comment("g3a_grow for ling_imm")
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  {
                    comment("Calculate increase in length/weight for each lengthgroup")
                    ling_imm__growth_l <- growth_bbinom((param[["ling.Linf"]] - ling_imm__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)), ling_imm__dl, length(ling_imm__dl), param[["ling.bbin"]] * 10)
                    ling_imm__growth_w <- param[["lingimm.walpha"]] * ((ling_imm__midlen - (param[["ling.Linf"]] - ling_imm__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)))^param[["lingimm.wbeta"]] - ling_imm__midlen^param[["lingimm.wbeta"]])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]
                    ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- g3a_grow_apply(ling_imm__growth_l, ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- (ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__growth_w)/pmax(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], 1e-05)
                  }
                }
            }
        }
        {
            comment("g3a_grow for ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  {
                    comment("Calculate increase in length/weight for each lengthgroup")
                    ling_mat__growth_l <- growth_bbinom((param[["ling.Linf"]] - ling_mat__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)), ling_mat__dl, length(ling_mat__dl), param[["ling.bbin"]] * 10)
                    ling_mat__growth_w <- param[["lingmat.walpha"]] * ((ling_mat__midlen - (param[["ling.Linf"]] - ling_mat__midlen) * (1 - exp(-(param[["ling.k"]] * 0.001) * cur_step_len)))^param[["lingmat.wbeta"]] - ling_mat__midlen^param[["lingmat.wbeta"]])
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]
                    ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- g3a_grow_apply(ling_mat__growth_l, ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] + ling_mat__growth_w)/pmax(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 1e-05)
                  }
                }
            }
        }
        {
            comment("g3a_mature for ling_imm")
            matured_ling_imm__wgt <- ling_imm__wgt
            for (age in seq(ling_imm__minage, ling_imm__maxage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  area <- ling_imm__area
                  if (age >= matured_ling_imm__minage && age <= matured_ling_imm__maxage) {
                    matured_ling_imm__age_idx <- age - matured_ling_imm__minage + 1
                    if (area == matured_ling_imm__area) {
                      if (TRUE) {
                        comment("Move matured ling_imm into temporary storage")
                        ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] - (matured_ling_imm__num[, matured_ling_imm__area_idx, matured_ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * 1)
                      }
                    }
                  }
                }
            }
        }
        {
            comment("Move matured ling_imm to ling_mat")
            for (age in seq(ling_mat__minage, ling_mat__maxage)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1
                for (ling_mat__area_idx in seq_along(ling_mat__areas)) {
                  area <- ling_mat__areas[[ling_mat__area_idx]]
                  if (age >= matured_ling_imm__minage && age <= matured_ling_imm__maxage) {
                    matured_ling_imm__age_idx <- age - matured_ling_imm__minage + 1
                    if (area == matured_ling_imm__area) {
                      if (TRUE) {
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + (matured_ling_imm__wgt[, matured_ling_imm__area_idx, matured_ling_imm__age_idx] * matured_ling_imm__num[, matured_ling_imm__area_idx, matured_ling_imm__age_idx] * 1)
                        ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + (matured_ling_imm__num[, matured_ling_imm__area_idx, matured_ling_imm__age_idx] * 1)
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/pmax(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], 1e-05)
                      }
                    }
                  }
                }
            }
        }
        if (cur_step_final) {
            comment("g3a_age for ling_imm")
            for (age in seq(ling_imm__maxage, ling_imm__minage)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1
                {
                  if (age == ling_imm__maxage) {
                    comment("TODO: Plus group migration shenanigans")
                  }
                  else {
                    ling_imm__num[, , ling_imm__age_idx + 1] <- ling_imm__num[, , ling_imm__age_idx + 1] + ling_imm__num[, , ling_imm__age_idx]
                    ling_imm__num[, , ling_imm__age_idx] <- 0
                  }
                }
            }
        }
    }
    stop("Should have return()ed somewhere in the loop")
}, class = c("g3_r", "function"))
