structure(function (param) 
{
    stopifnot("retro_years" %in% names(param))
    stopifnot("fish.init.scalar" %in% names(param))
    stopifnot("fish.init.1" %in% names(param))
    stopifnot("fish.init.2" %in% names(param))
    stopifnot("fish.init.3" %in% names(param))
    stopifnot("fish.init.4" %in% names(param))
    stopifnot("fish.init.5" %in% names(param))
    stopifnot("fish.init.6" %in% names(param))
    stopifnot("fish.init.7" %in% names(param))
    stopifnot("fish.init.8" %in% names(param))
    stopifnot("fish.init.9" %in% names(param))
    stopifnot("fish.init.10" %in% names(param))
    stopifnot("fish.M" %in% names(param))
    stopifnot("init.F" %in% names(param))
    stopifnot("recage" %in% names(param))
    stopifnot("fish.Linf" %in% names(param))
    stopifnot("fish.K" %in% names(param))
    stopifnot("fish.t0" %in% names(param))
    stopifnot("fish.deltat" %in% names(param))
    stopifnot("fish.init.sd" %in% names(param))
    stopifnot("fish.walpha" %in% names(param))
    stopifnot("fish.wbeta" %in% names(param))
    stopifnot("report_detail" %in% names(param))
    stopifnot("fish.comm.alpha" %in% names(param))
    stopifnot("fish.comm.l50" %in% names(param))
    stopifnot("fish.bbin" %in% names(param))
    stopifnot("fish.rec.scalar" %in% names(param))
    stopifnot("fish.rec.1990" %in% names(param))
    stopifnot("fish.rec.1991" %in% names(param))
    stopifnot("fish.rec.1992" %in% names(param))
    stopifnot("fish.rec.1993" %in% names(param))
    stopifnot("fish.rec.1994" %in% names(param))
    stopifnot("fish.rec.1995" %in% names(param))
    stopifnot("fish.rec.1996" %in% names(param))
    stopifnot("fish.rec.1997" %in% names(param))
    stopifnot("fish.rec.1998" %in% names(param))
    stopifnot("fish.rec.1999" %in% names(param))
    stopifnot("fish.rec.2000" %in% names(param))
    stopifnot("fish.rec.sd" %in% names(param))
    stopifnot("adist_surveyindices_log_acoustic_dist_weight" %in% names(param))
    stopifnot("cdist_sumofsquares_comm_ldist_weight" %in% names(param))
    assert_msg <- function(expr, message) {
        if (isFALSE(expr)) {
            warning(message)
            return(TRUE)
        }
        return(FALSE)
    }
    nvl <- function(...) {
        for (i in seq_len(...length())) if (!is.null(...elt(i))) 
            return(...elt(i))
        return(NULL)
    }
    normalize_vec <- function(a) {
        a/sum(a)
    }
    Rprintf <- function(...) {
        cat(sprintf(...))
    }
    avoid_zero_vec <- function(a) {
        (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
    }
    intlookup_getdefault <- function(lookup, key, def) {
        if (is.environment(lookup)) {
            out <- lookup[[as.character(key)]]
        }
        else {
            out <- if (key < 1) 
                NULL
            else lookup[key][[1]]
        }
        return(if (is.null(out)) def else out)
    }
    logspace_add_vec <- function(a, b) {
        pmax(a, b) + log1p(exp(pmin(a, b) - pmax(a, b)))
    }
    growth_bbinom <- function(delt_l, binn, beta) {
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
    avoid_zero <- function(a) {
        (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
    }
    g3a_grow_weightsimple_vec_rotate <- function(vec, a) {
        out <- vapply(seq_len(a), function(i) vec[i:(i + length(vec) - 1)], numeric(length(vec)))
        out[is.na(out)] <- vec[length(vec)]
        out
    }
    pow_vec <- function(a, b) {
        a^b
    }
    g3a_grow_weightsimple_vec_extrude <- function(vec, a) {
        array(vec, dim = c(length(vec), a))
    }
    g3a_grow_apply <- function(delta_l, delta_w, input_num, input_wgt) {
        na <- dim(delta_l)[[1]]
        n <- dim(delta_l)[[2]] - 1
        avoid_zero_vec <- function(a) {
            (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
        }
        growth.matrix <- array(0, c(na, na))
        wgt.matrix <- array(0, c(na, na))
        for (lg in 1:na) {
            if (lg == na) {
                growth.matrix[na, na] <- sum(delta_l[lg, ])
                wgt.matrix[lg, lg:na] <- delta_w[lg, 1:(na - lg + 1)]
            }
            else if (lg + n > na) {
                growth.matrix[lg, lg:(na - 1)] <- delta_l[lg, 1:(na - lg)]
                growth.matrix[lg, na] <- sum(delta_l[lg, (na - lg + 1):(n + 1)])
                wgt.matrix[lg, lg:na] <- delta_w[lg, 1:(na - lg + 1)]
            }
            else {
                growth.matrix[lg, lg:(n + lg)] <- delta_l[lg, ]
                wgt.matrix[lg, lg:(n + lg)] <- delta_w[lg, ]
            }
        }
        growth.matrix <- growth.matrix * as.vector(input_num)
        wgt.matrix <- growth.matrix * (wgt.matrix + as.vector(input_wgt))
        growth.matrix.sum <- colSums(growth.matrix)
        return(array(c(growth.matrix.sum, colSums(wgt.matrix)/avoid_zero_vec(growth.matrix.sum)), dim = c(na, 2)))
    }
    ratio_add_vec <- function(orig_vec, orig_amount, new_vec, new_amount) {
        (orig_vec * orig_amount + new_vec * new_amount)/avoid_zero_vec(orig_amount + new_amount)
    }
    g3_matrix_vec <- function(tf, vec) {
        return((tf %*% vec)[, 1])
    }
    surveyindices_linreg <- function(N, I, fixed_alpha, fixed_beta) {
        meanI <- mean(I)
        meanN <- mean(N)
        beta <- if (is.nan(fixed_beta)) 
            sum((I - meanI) * (N - meanN))/avoid_zero(sum((N - meanN)^2))
        else fixed_beta
        alpha <- if (is.nan(fixed_alpha)) 
            meanI - beta * meanN
        else fixed_alpha
        return(c(alpha, beta))
    }
    REPORT <- function(var) {
        var_name <- as.character(sys.call()[[2]])
        attr(nll, var_name) <<- if (var_name == "nll") 
            as.vector(var)
        else var
    }
    cur_time <- -1L
    stopifnot("project_years" %in% names(param))
    project_years <- param[["project_years"]]
    nll <- 0
    step_lengths <- 12L
    end_year <- 2000L
    retro_years <- param[["retro_years"]]
    start_year <- 1990L
    total_steps <- length(step_lengths) * (end_year - retro_years - start_year + project_years) + length(step_lengths) - 1L
    cur_year <- 0L
    step_count <- length(step_lengths)
    cur_year_projection <- FALSE
    cur_step <- 0L
    cur_step_final <- FALSE
    fish__minage <- 1L
    fish__maxage <- 10L
    fish__area <- 1L
    fish__num <- array(0, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    fish__wgt <- array(1, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    nan_fish__num <- FALSE
    nan_fish__wgt <- FALSE
    as_integer <- as.integer
    total_years <- end_year - retro_years - start_year + project_years + 1L
    detail_fish__num <- array(0, dim = c(length = 6L, area = 1L, age = 10L, time = as_integer(total_steps + 1)), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10"), time = sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years))))
    detail_fish__wgt <- array(1, dim = c(length = 6L, area = 1L, age = 10L, time = as_integer(total_steps + 1)), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10"), time = sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years))))
    comm__catch <- array(NA, dim = c(area = 1L), dimnames = list(area = "all"))
    comm__catchnum <- array(NA, dim = c(area = 1L), dimnames = list(area = "all"))
    fish__totalpredate <- array(NA, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    fish__predby_comm <- array(NA, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    comm__area <- 1L
    fish__suit_comm <- array(0, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    intlookup_zip <- function(keys, values) {
        if (min(keys) > 0 && max(keys) < 1e+05) {
            out <- list()
            out[as.integer(keys)] <- as.list(values)
        }
        else {
            out <- as.list(values)
            names(out) <- keys
            out <- as.environment(out)
        }
        attr(out, "key_var") <- deparse(sys.call()[[2]])
        attr(out, "value_var") <- deparse(sys.call()[[3]])
        return(out)
    }
    comm_landings__lookup <- intlookup_zip(comm_landings__keys, comm_landings__values)
    fish__consratio <- array(NA, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    fish__overconsumption <- structure(0, desc = "Total overconsumption of fish")
    cur_step_size <- step_lengths[[1]]/12
    fish__growth_lastcalc <- -1L
    fish__growth_l <- array(NA, dim = c(length = 6L, delta = 6L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), delta = c("0", "1", "2", "3", "4", "5")))
    fish__plusdl <- 10
    fish__growth_w <- array(NA, dim = c(length = 6L, delta = 6L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), delta = c("0", "1", "2", "3", "4", "5")))
    fish__prevtotal <- 0
    fish__renewalnum <- array(0, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    fish__renewalwgt <- array(0, dim = c(length = 6L, area = 1L, age = 10L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10")))
    adist_surveyindices_log_acoustic_dist_model__area <- 1L
    times_adist_surveyindices_log_acoustic_dist_model__lookup <- intlookup_zip(times_adist_surveyindices_log_acoustic_dist_model__keys, times_adist_surveyindices_log_acoustic_dist_model__values)
    adist_surveyindices_log_acoustic_dist_model__wgt <- array(0, dim = c(length = 1L, time = 11L, area = 1L), dimnames = list(length = "0:Inf", time = c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000"), area = "all"))
    fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix <- array(1, dim = c(1L, 6L), dimnames = NULL)
    adist_surveyindices_log_acoustic_dist_obs__area <- 1L
    nll_adist_surveyindices_log_acoustic_dist__wgt <- array(0, dim = c(time = as_integer(total_steps + 1)), dimnames = list(time = head(sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
    nll_adist_surveyindices_log_acoustic_dist__weight <- array(0, dim = c(time = as_integer(total_steps + 1)), dimnames = list(time = head(sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
    cdist_sumofsquares_comm_ldist_model__area <- 1L
    times_cdist_sumofsquares_comm_ldist_model__lookup <- intlookup_zip(times_cdist_sumofsquares_comm_ldist_model__keys, times_cdist_sumofsquares_comm_ldist_model__values)
    cdist_sumofsquares_comm_ldist_model__wgt <- array(0, dim = c(length = 5L, time = 11L, area = 1L), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:Inf"), time = c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000"), area = "all"))
    cdist_sumofsquares_comm_ldist_obs__area <- 1L
    times_cdist_sumofsquares_comm_ldist_obs__lookup <- intlookup_zip(times_cdist_sumofsquares_comm_ldist_obs__keys, times_cdist_sumofsquares_comm_ldist_obs__values)
    nll_cdist_sumofsquares_comm_ldist__wgt <- array(0, dim = c(time = as_integer(total_steps + 1)), dimnames = list(time = head(sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
    nll_cdist_sumofsquares_comm_ldist__weight <- array(0, dim = c(time = as_integer(total_steps + 1)), dimnames = list(time = head(sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
    g3l_understocking_total <- 0
    nll_understocking__wgt <- array(0, dim = c(time = as_integer(total_steps + 1)), dimnames = list(time = head(sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
    nll_understocking__weight <- array(0, dim = c(time = as_integer(total_steps + 1)), dimnames = list(time = head(sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years)), as_integer(total_steps + 1))))
    detail_fish__predby_comm <- array(NA, dim = c(length = 6L, area = 1L, age = 10L, time = as_integer(total_steps + 1)), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10"), time = sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years))))
    detail_fish__renewalnum <- array(0, dim = c(length = 6L, area = 1L, age = 10L, time = as_integer(total_steps + 1)), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10"), time = sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years))))
    detail_fish__suit_comm <- array(0, dim = c(length = 6L, area = 1L, age = 10L, time = as_integer(total_steps + 1)), dimnames = list(length = c("50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"), area = "all", age = c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10"), time = sprintf("%d-%02d", rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)), rep(seq_along(step_lengths), times = total_years))))
    while (TRUE) {
        {
            comment("g3a_time: Start of time period")
            cur_time <- cur_time + 1L
            if (cur_time == 0 && assert_msg(param[["retro_years"]] >= 0, "retro_years must be >= 0")) 
                return(NaN)
            if (cur_time == 0 && assert_msg(project_years >= 0, "project_years must be >= 0")) 
                return(NaN)
            if (FALSE) 
                assert_msg(is.finite(nll), "g3a_time: nll became NaN/Inf in previous timestep")
            if (cur_time > total_steps) {
                {
                  REPORT(adist_surveyindices_log_acoustic_dist_model__params)
                  REPORT(adist_surveyindices_log_acoustic_dist_model__wgt)
                  REPORT(cdist_sumofsquares_comm_ldist_model__wgt)
                  REPORT(comm__catch)
                  REPORT(comm__catchnum)
                  REPORT(cur_step)
                  REPORT(cur_step_final)
                  REPORT(cur_time)
                  REPORT(cur_year)
                  REPORT(cur_year_projection)
                  REPORT(detail_fish__num)
                  REPORT(detail_fish__predby_comm)
                  REPORT(detail_fish__renewalnum)
                  REPORT(detail_fish__suit_comm)
                  REPORT(detail_fish__wgt)
                  REPORT(fish__consratio)
                  REPORT(fish__growth_l)
                  REPORT(fish__growth_lastcalc)
                  REPORT(fish__growth_w)
                  REPORT(fish__num)
                  REPORT(fish__overconsumption)
                  REPORT(fish__predby_comm)
                  REPORT(fish__prevtotal)
                  REPORT(fish__renewalnum)
                  REPORT(fish__renewalwgt)
                  REPORT(fish__suit_comm)
                  REPORT(fish__totalpredate)
                  REPORT(fish__wgt)
                  REPORT(g3l_understocking_total)
                  REPORT(nan_fish__num)
                  REPORT(nan_fish__wgt)
                  REPORT(nll)
                  REPORT(nll_adist_surveyindices_log_acoustic_dist__weight)
                  REPORT(nll_adist_surveyindices_log_acoustic_dist__wgt)
                  REPORT(nll_cdist_sumofsquares_comm_ldist__weight)
                  REPORT(nll_cdist_sumofsquares_comm_ldist__wgt)
                  REPORT(nll_understocking__weight)
                  REPORT(nll_understocking__wgt)
                }
                return(nll)
            }
            cur_year <- start_year + (cur_time%/%step_count)
            cur_year_projection <- cur_year > end_year - param[["retro_years"]]
            cur_step <- (cur_time%%step_count) + 1L
            cur_step_final <- cur_step == step_count
        }
        {
            comment("g3a_initialconditions for fish")
            for (age in seq(fish__minage, fish__maxage, by = 1)) if (cur_time == 0L) {
                fish__age_idx <- age - fish__minage + 1L
                {
                  area <- fish__area
                  fish__area_idx <- (1L)
                  factor <- (param[["fish.init.scalar"]] * nvl(param[[paste("fish.init", age, sep = ".")]], {
                    warning("No value found in g3_param_table fish.init, ifmissing not specified")
                    NaN
                  }) * exp(-1 * (param[["fish.M"]] + param[["init.F"]]) * (age - param[["recage"]])))
                  dnorm <- ((fish__midlen - (param[["fish.Linf"]] * (1 - exp(-1 * param[["fish.K"]] * (age - (param[["fish.t0"]] + param[["fish.deltat"]]))))))/param[["fish.init.sd"]])
                  {
                    fish__num[, fish__area_idx, fish__age_idx] <- normalize_vec(exp(-(dnorm^2) * 0.5)) * 10000 * factor
                    fish__wgt[, fish__area_idx, fish__age_idx] <- param[["fish.walpha"]] * fish__midlen^param[["fish.wbeta"]]
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3a_initialconditions for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_initialconditions for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_initialconditions for fish'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3a_trace_nan: g3a_time: Start of time period")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_time: Start of time period'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_time: Start of time period'\n", cur_year, cur_step)
            }
        }
        if (param[["report_detail"]] == 1) 
            detail_fish__num[, , , cur_time + 1] <- fish__num
        if (param[["report_detail"]] == 1) 
            detail_fish__wgt[, , , cur_time + 1] <- fish__wgt
        {
            comment("Zero biomass-caught counter for comm")
            comm__catch[] <- 0
            comm__catchnum[] <- 0
        }
        {
            comment("g3a_trace_nan: Zero biomass-caught counter for comm")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Zero biomass-caught counter for comm'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Zero biomass-caught counter for comm'\n", cur_year, cur_step)
            }
        }
        {
            comment("Zero total predation counter for fish")
            fish__totalpredate[] <- 0
        }
        {
            comment("g3a_trace_nan: Zero total predation counter for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Zero total predation counter for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Zero total predation counter for fish'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3a_predate_fleet for fish")
            comment("Zero comm-fish biomass-consuming counter")
            fish__predby_comm[] <- 0
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                if (area == comm__area) {
                  comm__area_idx <- (1L)
                  fleet_area <- area
                  {
                    comment("Collect all suitable fish biomass for comm")
                    fish__suit_comm[, fish__area_idx, fish__age_idx] <- 1/(1 + exp(-param[["fish.comm.alpha"]] * (fish__midlen - param[["fish.comm.l50"]])))
                    fish__predby_comm[, fish__area_idx, fish__age_idx] <- fish__suit_comm[, fish__area_idx, fish__age_idx] * fish__num[, fish__area_idx, fish__age_idx] * fish__wgt[, fish__area_idx, fish__age_idx]
                    comm__catch[comm__area_idx] <- comm__catch[comm__area_idx] + sum(fish__predby_comm[, fish__area_idx, fish__age_idx])
                    comm__catchnum[comm__area_idx] <- comm__catchnum[comm__area_idx] + sum(fish__suit_comm[, fish__area_idx, fish__age_idx] * fish__num[, fish__area_idx, fish__age_idx])
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3a_predate_fleet for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_predate_fleet for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_predate_fleet for fish'\n", cur_year, cur_step)
            }
        }
        {
            comment("Scale comm catch of fish by total expected catch")
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                if (area == comm__area) {
                  comm__area_idx <- (1L)
                  fleet_area <- area
                  {
                    fish__predby_comm[, fish__area_idx, fish__age_idx] <- (fish__predby_comm[, fish__area_idx, fish__age_idx]/avoid_zero_vec(fish__wgt[, fish__area_idx, fish__age_idx])) * ((if (area != 1L) 
                      0
                    else intlookup_getdefault(comm_landings__lookup, cur_year, 0))/comm__catchnum[comm__area_idx])
                    fish__totalpredate[, fish__area_idx, fish__age_idx] <- fish__totalpredate[, fish__area_idx, fish__age_idx] + fish__predby_comm[, fish__area_idx, fish__age_idx]
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: Scale comm catch of fish by total expected catch")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Scale comm catch of fish by total expected catch'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Scale comm catch of fish by total expected catch'\n", cur_year, cur_step)
            }
        }
        {
            comment("Temporarily convert to being proportion of totalpredate")
            fish__predby_comm <- fish__predby_comm/avoid_zero_vec(fish__totalpredate)
        }
        {
            comment("g3a_trace_nan: Temporarily convert to being proportion of totalpredate")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Temporarily convert to being proportion of totalpredate'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Temporarily convert to being proportion of totalpredate'\n", cur_year, cur_step)
            }
        }
        {
            comment("Calculate fish overconsumption coefficient")
            fish__consratio <- fish__totalpredate/avoid_zero_vec(fish__num * fish__wgt)
            fish__consratio <- logspace_add_vec(fish__consratio * -1000, 0.95 * -1000)/-1000
            if (FALSE) 
                assert_msg(~all(fish__consratio <= 1), "g3a_predate_fleet: fish__consratio <= 1, can't consume more fish than currently exist")
            comment("Apply overconsumption to prey")
            fish__overconsumption <- sum(fish__totalpredate)
            fish__totalpredate <- (fish__num * fish__wgt) * fish__consratio
            fish__overconsumption <- fish__overconsumption - sum(fish__totalpredate)
            fish__num <- fish__num * (1 - fish__consratio)
        }
        {
            comment("g3a_trace_nan: Calculate fish overconsumption coefficient")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Calculate fish overconsumption coefficient'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Calculate fish overconsumption coefficient'\n", cur_year, cur_step)
            }
        }
        {
            comment("Zero comm catch before working out post-adjustment value")
            comm__catch[] <- 0
            comm__catchnum[] <- 0
        }
        {
            comment("Revert to being total biomass (applying overconsumption in process)")
            fish__predby_comm <- fish__predby_comm * fish__totalpredate
            comment("Update total catch")
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                if (area == comm__area) {
                  comm__area_idx <- (1L)
                  fleet_area <- area
                  {
                    comm__catch[comm__area_idx] <- comm__catch[comm__area_idx] + sum(fish__predby_comm[, fish__area_idx, fish__age_idx])
                    comm__catchnum[comm__area_idx] <- comm__catchnum[comm__area_idx] + sum(fish__predby_comm[, fish__area_idx, fish__age_idx]/fish__wgt[, fish__area_idx, fish__age_idx])
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: Revert to being total biomass (applying overconsumption in process)")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Revert to being total biomass (applying overconsumption in process)'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Revert to being total biomass (applying overconsumption in process)'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3a_trace_nan: Zero comm catch before working out post-adjustment value")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Zero comm catch before working out post-adjustment value'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Zero comm catch before working out post-adjustment value'\n", cur_year, cur_step)
            }
        }
        {
            comment("Natural mortality for fish")
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                fish__num[, fish__area_idx, fish__age_idx] <- fish__num[, fish__area_idx, fish__age_idx] * exp(-(param[["fish.M"]]) * cur_step_size)
            }
        }
        {
            comment("g3a_trace_nan: Natural mortality for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Natural mortality for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Natural mortality for fish'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3a_grow for fish")
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                {
                  if (fish__growth_lastcalc != floor(cur_step_size * 12L)) {
                    comment("Calculate length/weight delta matrices for current lengthgroups")
                    fish__growth_l[] <- growth_bbinom(avoid_zero_vec(avoid_zero_vec((param[["fish.Linf"]] - fish__midlen) * (1 - exp(-(param[["fish.K"]]) * cur_step_size)))/fish__plusdl), 5L, avoid_zero(param[["fish.bbin"]]))
                    fish__growth_w[] <- (g3a_grow_weightsimple_vec_rotate(pow_vec(fish__midlen, param[["fish.wbeta"]]), 5L + 1) - g3a_grow_weightsimple_vec_extrude(pow_vec(fish__midlen, param[["fish.wbeta"]]), 5L + 1)) * param[["fish.walpha"]]
                    comment("Don't recalculate until cur_step_size changes")
                    fish__growth_lastcalc <- floor(cur_step_size * 12L)
                  }
                  if (FALSE) 
                    fish__prevtotal <- sum(fish__num[, fish__area_idx, fish__age_idx])
                  comment("Update fish using delta matrices")
                  {
                    growthresult <- g3a_grow_apply(fish__growth_l, fish__growth_w, fish__num[, fish__area_idx, fish__age_idx], fish__wgt[, fish__area_idx, fish__age_idx])
                    {
                      fish__num[, fish__area_idx, fish__age_idx] <- growthresult[, (1)]
                      fish__wgt[, fish__area_idx, fish__age_idx] <- growthresult[, (2)]
                    }
                  }
                  if (FALSE) 
                    assert_msg(~abs(fish__prevtotal - sum(fish__num[, fish__area_idx, fish__age_idx])) < 1e-04, "g3a_growmature: fish__num totals are not the same before and after growth")
                }
            }
        }
        {
            comment("g3a_trace_nan: g3a_grow for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_grow for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_grow for fish'\n", cur_year, cur_step)
            }
        }
        {
            factor <- (param[["fish.rec.scalar"]] * nvl(param[[paste("fish.rec", cur_year, sep = ".")]], NaN))
            {
                comment("g3a_renewal for fish")
                for (age in seq(fish__minage, fish__maxage, by = 1)) if (age == fish__minage && cur_step == 1 && (!cur_year_projection)) {
                  fish__age_idx <- age - fish__minage + 1L
                  area <- fish__area
                  fish__area_idx <- (1L)
                  dnorm <- ((fish__midlen - (param[["fish.Linf"]] * (1 - exp(-1 * param[["fish.K"]] * (age - (param[["fish.t0"]] + param[["fish.deltat"]]))))))/param[["fish.rec.sd"]])
                  {
                    fish__renewalnum[, fish__area_idx, fish__age_idx] <- normalize_vec(exp(-(dnorm^2) * 0.5)) * 10000 * factor
                    fish__renewalwgt[, fish__area_idx, fish__age_idx] <- param[["fish.walpha"]] * fish__midlen^param[["fish.wbeta"]]
                    comment("Add result to fish")
                    fish__wgt[, fish__area_idx, fish__age_idx] <- ratio_add_vec(fish__wgt[, fish__area_idx, fish__age_idx], fish__num[, fish__area_idx, fish__age_idx], fish__renewalwgt[, fish__area_idx, fish__age_idx], fish__renewalnum[, fish__area_idx, fish__age_idx])
                    fish__num[, fish__area_idx, fish__age_idx] <- fish__num[, fish__area_idx, fish__age_idx] + fish__renewalnum[, fish__area_idx, fish__age_idx]
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3a_renewal for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_renewal for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_renewal for fish'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist")
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                if (area == adist_surveyindices_log_acoustic_dist_model__area) {
                  adist_surveyindices_log_acoustic_dist_model__area_idx <- (1L)
                  adist_surveyindices_log_acoustic_dist_model__time_idx <- intlookup_getdefault(times_adist_surveyindices_log_acoustic_dist_model__lookup, (cur_year * 100L + cur_step * 0L), -1L)
                  if (adist_surveyindices_log_acoustic_dist_model__time_idx >= (1L)) {
                    comment("Take fish total biomass to our count")
                    adist_surveyindices_log_acoustic_dist_model__wgt[, adist_surveyindices_log_acoustic_dist_model__time_idx, adist_surveyindices_log_acoustic_dist_model__area_idx] <- adist_surveyindices_log_acoustic_dist_model__wgt[, adist_surveyindices_log_acoustic_dist_model__time_idx, adist_surveyindices_log_acoustic_dist_model__area_idx] + g3_matrix_vec(fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix, (fish__num[, fish__area_idx, fish__age_idx] * fish__wgt[, fish__area_idx, fish__age_idx]))
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist'\n", cur_year, cur_step)
            }
        }
        {
            adist_surveyindices_log_acoustic_dist_model__max_time_idx <- (11L)
            {
                comment("g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs")
                if (cur_step_final) {
                  area <- adist_surveyindices_log_acoustic_dist_model__area
                  adist_surveyindices_log_acoustic_dist_model__area_idx <- (1L)
                  adist_surveyindices_log_acoustic_dist_model__time_idx <- intlookup_getdefault(times_adist_surveyindices_log_acoustic_dist_model__lookup, (cur_year * 100L + cur_step * 0L), -1L)
                  if (adist_surveyindices_log_acoustic_dist_model__time_idx >= (1L)) 
                    if (area == adist_surveyindices_log_acoustic_dist_obs__area) {
                      adist_surveyindices_log_acoustic_dist_obs__area_idx <- (1L)
                      {
                        adist_surveyindices_log_acoustic_dist_model__params <- if (adist_surveyindices_log_acoustic_dist_model__time_idx != adist_surveyindices_log_acoustic_dist_model__max_time_idx) 
                          adist_surveyindices_log_acoustic_dist_model__params
                        else surveyindices_linreg(log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_model__wgt[, , adist_surveyindices_log_acoustic_dist_model__area_idx])), log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_obs__wgt[, , adist_surveyindices_log_acoustic_dist_obs__area_idx])), NaN, NaN)
                        {
                          cur_cdist_nll <- if (adist_surveyindices_log_acoustic_dist_model__time_idx != adist_surveyindices_log_acoustic_dist_model__max_time_idx) 
                            0
                          else sum((adist_surveyindices_log_acoustic_dist_model__params[[1]] + adist_surveyindices_log_acoustic_dist_model__params[[2]] * log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_model__wgt[, , adist_surveyindices_log_acoustic_dist_model__area_idx])) - log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_obs__wgt[, , adist_surveyindices_log_acoustic_dist_obs__area_idx])))^2)
                          {
                            nll <- nll + param[["adist_surveyindices_log_acoustic_dist_weight"]] * cur_cdist_nll
                            nll_adist_surveyindices_log_acoustic_dist__wgt[cur_time + 1L] <- nll_adist_surveyindices_log_acoustic_dist__wgt[cur_time + 1L] + cur_cdist_nll
                            nll_adist_surveyindices_log_acoustic_dist__weight[cur_time + 1L] <- param[["adist_surveyindices_log_acoustic_dist_weight"]]
                            REPORT(adist_surveyindices_log_acoustic_dist_obs__wgt)
                          }
                        }
                      }
                    }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist")
            for (age in seq(fish__minage, fish__maxage, by = 1)) {
                fish__age_idx <- age - fish__minage + 1L
                area <- fish__area
                fish__area_idx <- (1L)
                if (area == cdist_sumofsquares_comm_ldist_model__area) {
                  cdist_sumofsquares_comm_ldist_model__area_idx <- (1L)
                  cdist_sumofsquares_comm_ldist_model__time_idx <- intlookup_getdefault(times_cdist_sumofsquares_comm_ldist_model__lookup, (cur_year * 100L + cur_step * 0L), -1L)
                  if (cdist_sumofsquares_comm_ldist_model__time_idx >= (1L)) {
                    comment("Take prey_stock__predby_fleet_stock weight, add to our count")
                    cdist_sumofsquares_comm_ldist_model__wgt[, cdist_sumofsquares_comm_ldist_model__time_idx, cdist_sumofsquares_comm_ldist_model__area_idx] <- cdist_sumofsquares_comm_ldist_model__wgt[, cdist_sumofsquares_comm_ldist_model__time_idx, cdist_sumofsquares_comm_ldist_model__area_idx] + g3_matrix_vec(fish_cdist_sumofsquares_comm_ldist_model_lgmatrix, fish__predby_comm[, fish__area_idx, fish__age_idx])
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs")
            if (cur_step_final) {
                area <- cdist_sumofsquares_comm_ldist_model__area
                cdist_sumofsquares_comm_ldist_model__area_idx <- (1L)
                cdist_sumofsquares_comm_ldist_model__time_idx <- intlookup_getdefault(times_cdist_sumofsquares_comm_ldist_model__lookup, (cur_year * 100L + cur_step * 0L), -1L)
                if (cdist_sumofsquares_comm_ldist_model__time_idx >= (1L)) 
                  if (area == cdist_sumofsquares_comm_ldist_obs__area) {
                    cdist_sumofsquares_comm_ldist_model__sstotal <- avoid_zero(sum(cdist_sumofsquares_comm_ldist_model__wgt[, cdist_sumofsquares_comm_ldist_model__time_idx, cdist_sumofsquares_comm_ldist_model__area_idx]))
                    cdist_sumofsquares_comm_ldist_obs__area_idx <- (1L)
                    cdist_sumofsquares_comm_ldist_obs__time_idx <- intlookup_getdefault(times_cdist_sumofsquares_comm_ldist_obs__lookup, (cur_year * 100L + cur_step * 0L), -1L)
                    if (cdist_sumofsquares_comm_ldist_obs__time_idx >= (1L)) {
                      cdist_sumofsquares_comm_ldist_obs__sstotal <- avoid_zero(sum(cdist_sumofsquares_comm_ldist_obs__wgt[, cdist_sumofsquares_comm_ldist_obs__time_idx, cdist_sumofsquares_comm_ldist_obs__area_idx]))
                      cur_cdist_nll <- sum((((cdist_sumofsquares_comm_ldist_model__wgt[, cdist_sumofsquares_comm_ldist_model__time_idx, cdist_sumofsquares_comm_ldist_model__area_idx]/cdist_sumofsquares_comm_ldist_model__sstotal) - (cdist_sumofsquares_comm_ldist_obs__wgt[, cdist_sumofsquares_comm_ldist_obs__time_idx, cdist_sumofsquares_comm_ldist_obs__area_idx]/cdist_sumofsquares_comm_ldist_obs__sstotal))^2))
                      {
                        nll <- nll + param[["cdist_sumofsquares_comm_ldist_weight"]] * cur_cdist_nll
                        nll_cdist_sumofsquares_comm_ldist__wgt[cur_time + 1L] <- nll_cdist_sumofsquares_comm_ldist__wgt[cur_time + 1L] + cur_cdist_nll
                        nll_cdist_sumofsquares_comm_ldist__weight[cur_time + 1L] <- param[["cdist_sumofsquares_comm_ldist_weight"]]
                        REPORT(cdist_sumofsquares_comm_ldist_obs__wgt)
                      }
                    }
                  }
            }
        }
        {
            comment("g3a_trace_nan: g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs'\n", cur_year, cur_step)
            }
        }
        {
            comment("Reset understocking total")
            g3l_understocking_total <- 0
        }
        {
            comment("g3a_trace_nan: Reset understocking total")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'Reset understocking total'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'Reset understocking total'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3l_understocking for fish")
            comment("Add understocking from fish as biomass to nll")
            g3l_understocking_total <- g3l_understocking_total + fish__overconsumption
        }
        {
            comment("g3a_trace_nan: g3l_understocking for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_understocking for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_understocking for fish'\n", cur_year, cur_step)
            }
        }
        {
            comment("g3l_understocking: Combine and add to nll")
            g3l_understocking_total <- g3l_understocking_total^2
            nll <- nll + 1e+08 * g3l_understocking_total
            nll_understocking__wgt[cur_time + 1L] <- nll_understocking__wgt[cur_time + 1L] + g3l_understocking_total
            nll_understocking__weight[cur_time + 1L] <- 1e+08
        }
        {
            comment("g3a_trace_nan: g3l_understocking: Combine and add to nll")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_understocking: Combine and add to nll'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_understocking: Combine and add to nll'\n", cur_year, cur_step)
            }
        }
        if (param[["report_detail"]] == 1) 
            detail_fish__predby_comm[, , , cur_time + 1] <- fish__predby_comm
        if (param[["report_detail"]] == 1) 
            detail_fish__renewalnum[, , , cur_time + 1] <- fish__renewalnum
        if (param[["report_detail"]] == 1) 
            detail_fish__suit_comm[, , , cur_time + 1] <- fish__suit_comm
        if (cur_step_final) {
            comment("g3a_age for fish")
            for (age in seq(fish__maxage, fish__minage, by = -1)) {
                fish__age_idx <- age - fish__minage + 1L
                {
                  comment("Check stock has remained finite for this step")
                  if (FALSE) 
                    assert_msg(~all(is.finite(fish__num[, , fish__age_idx])), "fish__num became NaN/Inf in this timestep")
                  if (FALSE) 
                    assert_msg(~all(is.finite(fish__wgt[, , fish__age_idx])), "fish__wgt became NaN/Inf in this timestep")
                  if (age == fish__maxage) {
                    comment("Oldest fish is a plus-group, combine with younger individuals")
                    fish__wgt[, , fish__age_idx] <- ratio_add_vec(fish__wgt[, , fish__age_idx], fish__num[, , fish__age_idx], fish__wgt[, , fish__age_idx - 1L], fish__num[, , fish__age_idx - 1L])
                    fish__num[, , fish__age_idx] <- fish__num[, , fish__age_idx] + fish__num[, , fish__age_idx - 1L]
                  }
                  else if (age == fish__minage) {
                    comment("Empty youngest fish age-group")
                    fish__num[, , fish__age_idx] <- 0
                  }
                  else {
                    comment("Move fish age-group to next one up")
                    fish__num[, , fish__age_idx] <- fish__num[, , fish__age_idx - 1L]
                    fish__wgt[, , fish__age_idx] <- fish__wgt[, , fish__age_idx - 1L]
                  }
                }
            }
        }
        {
            comment("g3a_trace_nan: g3a_age for fish")
            if (!nan_fish__num && any(is.nan(fish__num))) {
                nan_fish__num <- TRUE
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_age for fish'\n", cur_year, cur_step)
            }
            if (!nan_fish__wgt && any(is.nan(fish__wgt))) {
                nan_fish__wgt <- TRUE
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_age for fish'\n", cur_year, cur_step)
            }
        }
    }
    stop("Should have return()ed somewhere in the loop")
}, class = c("g3_r", "function"), parameter_template = list(retro_years = 0, fish.init.scalar = 1, fish.init.1 = 1, fish.init.2 = 1, fish.init.3 = 1, fish.init.4 = 1, fish.init.5 = 1, fish.init.6 = 1, fish.init.7 = 1, fish.init.8 = 1, fish.init.9 = 1, fish.init.10 = 1, fish.M = 0, init.F = 0, recage = 0, fish.Linf = 1, fish.K = 1, fish.t0 = 0, fish.deltat = 0, fish.init.sd = 10, fish.walpha = 0, fish.wbeta = 0, report_detail = 0L, fish.comm.alpha = 0, fish.comm.l50 = 0, fish.bbin = 0, fish.rec.scalar = 0, 
    fish.rec.1990 = 0, fish.rec.1991 = 0, fish.rec.1992 = 0, fish.rec.1993 = 0, fish.rec.1994 = 0, fish.rec.1995 = 0, fish.rec.1996 = 0, fish.rec.1997 = 0, fish.rec.1998 = 0, fish.rec.1999 = 0, fish.rec.2000 = 0, fish.rec.sd = 10, adist_surveyindices_log_acoustic_dist_weight = 1, cdist_sumofsquares_comm_ldist_weight = 1, project_years = 0))
