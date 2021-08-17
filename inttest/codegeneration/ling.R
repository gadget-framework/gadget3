structure(function (param) 
{
    stopifnot("ling.Linf" %in% names(param))
    stopifnot("ling.k" %in% names(param))
    stopifnot("ling.recl" %in% names(param))
    stopifnot("lingimm.init.scalar" %in% names(param))
    stopifnot("lingimm.M" %in% names(param))
    stopifnot("ling.init.F" %in% names(param))
    stopifnot("lingimm.init" %in% names(param))
    stopifnot("lingimm.walpha" %in% names(param))
    stopifnot("lingimm.wbeta" %in% names(param))
    stopifnot("lingmat.init.scalar" %in% names(param))
    stopifnot("lingmat.M" %in% names(param))
    stopifnot("lingmat.init" %in% names(param))
    stopifnot("lingmat.walpha" %in% names(param))
    stopifnot("lingmat.wbeta" %in% names(param))
    stopifnot("ling.igfs.alpha" %in% names(param))
    stopifnot("ling.igfs.l50" %in% names(param))
    stopifnot("ling.bbin" %in% names(param))
    stopifnot("ling.mat1" %in% names(param))
    stopifnot("ling.mat2" %in% names(param))
    stopifnot("ling.rec.scalar" %in% names(param))
    stopifnot("ling.rec.1994" %in% names(param))
    stopifnot("ling.rec.1995" %in% names(param))
    stopifnot("ling.rec.1996" %in% names(param))
    stopifnot("ling.rec.1997" %in% names(param))
    stopifnot("ling.rec.1998" %in% names(param))
    stopifnot("ling.rec.1999" %in% names(param))
    stopifnot("ling.rec.2000" %in% names(param))
    stopifnot("ling.rec.2001" %in% names(param))
    stopifnot("ling.rec.2002" %in% names(param))
    stopifnot("ling.rec.2003" %in% names(param))
    stopifnot("ling.rec.2004" %in% names(param))
    stopifnot("ling.rec.2005" %in% names(param))
    stopifnot("ling.rec.2006" %in% names(param))
    stopifnot("ling.rec.2007" %in% names(param))
    stopifnot("ling.rec.2008" %in% names(param))
    stopifnot("ling.rec.2009" %in% names(param))
    stopifnot("ling.rec.2010" %in% names(param))
    stopifnot("ling.rec.2011" %in% names(param))
    stopifnot("ling.rec.2012" %in% names(param))
    stopifnot("ling.rec.2013" %in% names(param))
    stopifnot("ling.rec.2014" %in% names(param))
    stopifnot("ling.rec.2015" %in% names(param))
    stopifnot("ling.rec.2016" %in% names(param))
    stopifnot("ling.rec.2017" %in% names(param))
    stopifnot("ling.rec.2018" %in% names(param))
    stopifnot("ldist_lln_weight" %in% names(param))
    assert_msg <- function (expr, message) 
    {
        if (isFALSE(expr)) 
            stop(message)
    }
    Rprintf <- function (...) 
    {
        cat(sprintf(...))
    }
    inttypelookup_getdefault <- function (lookup, key, def) 
    {
        out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
        return(if (length(out) < 1) def else out)
    }
    avoid_zero_vec <- function (a) 
    {
        (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
    }
    logspace_add_vec <- function (a, b) 
    {
        pmax(a, b) + log1p(exp(pmin(a, b) - pmax(a, b)))
    }
    growth_bbinom <- function (delt_l, binn, beta) 
    {
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
    avoid_zero <- function (a) 
    {
        (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
    }
    g3a_grow_weightsimple_vec_rotate <- function (vec, a) 
    {
        out <- vapply(seq_len(a), function(i) vec[i:(i + length(vec) - 1)], numeric(length(vec)))
        out[is.na(out)] <- vec[length(vec)]
        out
    }
    pow_vec <- function (a, b) 
    {
        a^b
    }
    g3a_grow_weightsimple_vec_extrude <- function (vec, a) 
    {
        array(vec, dim = c(length(vec), a))
    }
    g3a_grow_apply <- function (delta_l, delta_w, input_num, input_wgt) 
    {
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
        return(array(c(Matrix::colSums(growth.matrix), Matrix::colSums(wgt.matrix)/avoid_zero_vec(Matrix::colSums(growth.matrix))), dim = c(na, 2)))
    }
    nvl <- function (...) 
    {
        for (i in seq_len(...length())) if (!is.null(...elt(i))) 
            return(...elt(i))
        return(NULL)
    }
    ratio_add_vec <- function (orig_vec, orig_amount, new_vec, new_amount) 
    {
        (orig_vec * orig_amount + new_vec * new_amount)/avoid_zero_vec(orig_amount + new_amount)
    }
    intintlookup_getdefault <- function (lookup, key, def) 
    {
        out <- lookup$values[which(lookup$keys == key, arr.ind = TRUE)]
        return(if (length(out) < 1) def else out)
    }
    cur_time <- -1L
    nll <- 0
    step_lengths <- model_data$step_lengths
    end_year <- 2018L
    start_year <- 1994L
    total_steps <- length(step_lengths) * (end_year - start_year) + length(step_lengths) - 1L
    cur_year <- 0L
    step_count <- length(step_lengths)
    cur_step <- 0L
    cur_step_final <- FALSE
    ling_imm__minage <- 3L
    ling_imm__maxage <- 10L
    ling_imm__area <- 1L
    ling_imm__num <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    ling_imm__midlen <- model_data$ling_imm__midlen
    ling_imm_stddev <- model_data$ling_imm_stddev
    ling_imm__wgt <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    ling_mat__minage <- 5L
    ling_mat__maxage <- 15L
    ling_mat__area <- 1L
    ling_mat__num <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", "age10", 
    "age11", "age12", "age13", "age14", "age15")))
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
    ling_imm__suit_igfs <- array(0, dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_mat__igfs <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", "age10", 
    "age11", "age12", "age13", "age14", "age15")))
    ling_mat__suit_igfs <- array(0, dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", 
    "age10", "age11", "age12", "age13", "age14", "age15")))
    inttypelookup_zip <- function (keys, values) 
    {
        list(keys = keys, values = values)
    }
    igfs_totaldata__keys <- model_data$igfs_totaldata__keys
    igfs_totaldata__values <- model_data$igfs_totaldata__values
    igfs_totaldata__lookup <- inttypelookup_zip(igfs_totaldata__keys, igfs_totaldata__values)
    ling_imm__consratio <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", "age8", 
    "age9", "age10")))
    ling_imm__overconsumption <- structure(0, desc = "Total overconsumption of ling_imm")
    ling_mat__consratio <- array(dim = c(length = 35L, area = 1L, age = 11L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age5", "age6", "age7", "age8", "age9", "age10", 
    "age11", "age12", "age13", "age14", "age15")))
    ling_mat__overconsumption <- structure(0, desc = "Total overconsumption of ling_mat")
    cur_step_size <- step_lengths[[1]]/12
    ling_imm__transitioning_num <- array(0, dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", 
    "age7", "age8", "age9", "age10")))
    ling_imm__transitioning_wgt <- array(dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_imm__growth_lastcalc <- -1L
    ling_imm__growth_l <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_imm__plusdl <- 4
    ling_imm__growth_w <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_imm__prevtotal <- 0
    ling_mat__growth_lastcalc <- -1L
    ling_mat__growth_l <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_mat__plusdl <- 4
    ling_mat__growth_w <- array(dim = c(0L, 0L), dimnames = NULL)
    ling_mat__prevtotal <- 0
    ling_imm__renewalnum <- array(0, dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    ling_imm__renewalwgt <- array(0, dim = c(length = 35L, area = 1L, age = 8L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = c("age3", "age4", "age5", "age6", "age7", 
    "age8", "age9", "age10")))
    cdist_ldist_lln_model__num <- array(0, dim = c(length = 35L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156")))
    intintlookup_zip <- function (keys, values) 
    {
        list(keys = keys, values = values)
    }
    times_cdist_ldist_lln_obs__keys <- model_data$times_cdist_ldist_lln_obs__keys
    times_cdist_ldist_lln_obs__values <- model_data$times_cdist_ldist_lln_obs__values
    times_cdist_ldist_lln_obs__lookup <- intintlookup_zip(times_cdist_ldist_lln_obs__keys, times_cdist_ldist_lln_obs__values)
    cdist_ldist_lln_obs__num <- model_data$cdist_ldist_lln_obs__num
    nll_cdist_ldist_lln__num <- array(0, dim = c(time = total_steps + 1), dimnames = list(time = sprintf("%d-%02d", rep(seq(start_year, end_year), each = length(step_lengths)), rep(seq_along(step_lengths), times = end_year - start_year + 1))))
    nll_cdist_ldist_lln__weight <- array(0, dim = c(time = total_steps + 1), dimnames = list(time = sprintf("%d-%02d", rep(seq(start_year, end_year), each = length(step_lengths)), rep(seq_along(step_lengths), times = end_year - start_year + 1))))
    g3l_understocking_total <- 0
    nll_understocking__wgt <- array(0, dim = c(time = total_steps + 1), dimnames = list(time = sprintf("%d-%02d", rep(seq(start_year, end_year), each = length(step_lengths)), rep(seq_along(step_lengths), times = end_year - start_year + 1))))
    nll_understocking__weight <- array(0, dim = c(time = total_steps + 1), dimnames = list(time = sprintf("%d-%02d", rep(seq(start_year, end_year), each = length(step_lengths)), rep(seq_along(step_lengths), times = end_year - start_year + 1))))
    ling_imm_movement__transitioning_num <- array(dim = c(length = 35L, area = 1L, age = 1L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = "age11"))
    ling_imm_movement__transitioning_wgt <- array(dim = c(length = 35L, area = 1L, age = 1L), dimnames = list(length = c("len20", "len24", "len28", "len32", "len36", "len40", "len44", "len48", "len52", "len56", "len60", "len64", "len68", "len72", "len76", "len80", "len84", "len88", "len92", "len96", "len100", "len104", "len108", "len112", "len116", "len120", "len124", "len128", "len132", "len136", "len140", "len144", "len148", "len152", "len156"), area = "area1", age = "age11"))
    ling_imm_movement__minage <- 11L
    ling_imm_movement__maxage <- 11L
    ling_imm_movement__area <- 1L
    while (TRUE) {
        {
            comment("g3a_time")
            cur_time <- cur_time + 1L
            if (TRUE) 
                assert_msg(is.finite(nll), "g3a_time: nll became NaN/Inf in previous timestep")
            if (cur_time > total_steps) {
                {
                  attr(nll, "cdist_ldist_lln_model__num") <- cdist_ldist_lln_model__num
                  attr(nll, "cur_step") <- cur_step
                  attr(nll, "cur_step_final") <- cur_step_final
                  attr(nll, "cur_time") <- cur_time
                  attr(nll, "cur_year") <- cur_year
                  attr(nll, "g3l_understocking_total") <- g3l_understocking_total
                  attr(nll, "igfs__catch") <- igfs__catch
                  attr(nll, "ling_imm__consratio") <- ling_imm__consratio
                  attr(nll, "ling_imm__growth_l") <- ling_imm__growth_l
                  attr(nll, "ling_imm__growth_lastcalc") <- ling_imm__growth_lastcalc
                  attr(nll, "ling_imm__growth_w") <- ling_imm__growth_w
                  attr(nll, "ling_imm__igfs") <- ling_imm__igfs
                  attr(nll, "ling_imm__num") <- ling_imm__num
                  attr(nll, "ling_imm__overconsumption") <- ling_imm__overconsumption
                  attr(nll, "ling_imm__prevtotal") <- ling_imm__prevtotal
                  attr(nll, "ling_imm__renewalnum") <- ling_imm__renewalnum
                  attr(nll, "ling_imm__renewalwgt") <- ling_imm__renewalwgt
                  attr(nll, "ling_imm__suit_igfs") <- ling_imm__suit_igfs
                  attr(nll, "ling_imm__totalpredate") <- ling_imm__totalpredate
                  attr(nll, "ling_imm__transitioning_num") <- ling_imm__transitioning_num
                  attr(nll, "ling_imm__transitioning_wgt") <- ling_imm__transitioning_wgt
                  attr(nll, "ling_imm__wgt") <- ling_imm__wgt
                  attr(nll, "ling_imm_movement__transitioning_num") <- ling_imm_movement__transitioning_num
                  attr(nll, "ling_imm_movement__transitioning_wgt") <- ling_imm_movement__transitioning_wgt
                  attr(nll, "ling_mat__consratio") <- ling_mat__consratio
                  attr(nll, "ling_mat__growth_l") <- ling_mat__growth_l
                  attr(nll, "ling_mat__growth_lastcalc") <- ling_mat__growth_lastcalc
                  attr(nll, "ling_mat__growth_w") <- ling_mat__growth_w
                  attr(nll, "ling_mat__igfs") <- ling_mat__igfs
                  attr(nll, "ling_mat__num") <- ling_mat__num
                  attr(nll, "ling_mat__overconsumption") <- ling_mat__overconsumption
                  attr(nll, "ling_mat__prevtotal") <- ling_mat__prevtotal
                  attr(nll, "ling_mat__suit_igfs") <- ling_mat__suit_igfs
                  attr(nll, "ling_mat__totalpredate") <- ling_mat__totalpredate
                  attr(nll, "ling_mat__wgt") <- ling_mat__wgt
                  attr(nll, "nll") <- nll[[1]]
                  attr(nll, "nll_cdist_ldist_lln__num") <- nll_cdist_ldist_lln__num
                  attr(nll, "nll_cdist_ldist_lln__weight") <- nll_cdist_ldist_lln__weight
                  attr(nll, "nll_understocking__weight") <- nll_understocking__weight
                  attr(nll, "nll_understocking__wgt") <- nll_understocking__wgt
                }
                return(nll)
            }
            cur_year <- start_year + (cur_time%/%step_count)
            cur_step <- (cur_time%%step_count) + 1L
            cur_step_final <- cur_step == step_count
            if (FALSE) 
                Rprintf("** Tick: %d-%d\n", cur_year, cur_step)
        }
        {
            ling_imm__area_idx <- (1)
            {
                comment("g3a_initialconditions_normalparam for ling_imm")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (cur_time == 0L) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  {
                    area <- ling_imm__area
                    {
                      comment("Calculate exp(-(dnorm**2) * 0.5)")
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(((ling_imm__midlen - ((param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))))) * (1/(ling_imm_stddev[[age - 3 + 1]])))^2) * 0.5)
                      comment("scale results")
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (10000/sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]))
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (param[["lingimm.init.scalar"]] * exp(-1 * (param[["lingimm.M"]] + param[["ling.init.F"]]) * age) * param[["lingimm.init"]][[age - 3 + 1]])
                      comment("Generate corresponding mean weight")
                      ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                    }
                  }
                }
            }
        }
        {
            ling_mat__area_idx <- (1)
            {
                comment("g3a_initialconditions_normalparam for ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (cur_time == 0L) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  {
                    area <- ling_mat__area
                    {
                      comment("Calculate exp(-(dnorm**2) * 0.5)")
                      ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- exp(-(((ling_mat__midlen - ((param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))))) * (1/(ling_mat_stddev[[age - 5 + 1]])))^2) * 0.5)
                      comment("scale results")
                      ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * (10000/sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]))
                      ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * (param[["lingmat.init.scalar"]] * exp(-1 * (param[["lingmat.M"]] + param[["ling.init.F"]]) * age) * param[["lingmat.init"]][[age - 5 + 1]])
                      comment("Generate corresponding mean weight")
                      ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- param[["lingmat.walpha"]] * ling_mat__midlen^param[["lingmat.wbeta"]]
                    }
                  }
                }
            }
        }
        {
            comment("Zero biomass-caught counter for igfs")
            igfs__catch[] <- 0
        }
        {
            comment("Zero total predation counter for ling_imm")
            ling_imm__totalpredate[] <- 0
        }
        {
            comment("Zero total predation counter for ling_mat")
            ling_mat__totalpredate[] <- 0
        }
        {
            igfs__area_idx <- (1)
            ling_imm__area_idx <- (1)
            {
                comment("g3a_predate_fleet for ling_imm")
                comment("Zero igfs-ling_imm biomass-consuming counter")
                ling_imm__igfs[] <- 0
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    comment("Collect all suitable ling_imm biomass for igfs")
                    ling_imm__suit_igfs[, ling_imm__area_idx, ling_imm__age_idx] <- 1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_imm__midlen - param[["ling.igfs.l50"]])))
                    ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__suit_igfs[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx]
                    igfs__catch[igfs__area_idx] <- igfs__catch[igfs__area_idx] + sum(ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx])
                  }
                }
            }
        }
        {
            igfs__area_idx <- (1)
            ling_mat__area_idx <- (1)
            {
                comment("g3a_predate_fleet for ling_mat")
                comment("Zero igfs-ling_mat biomass-consuming counter")
                ling_mat__igfs[] <- 0
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  area <- ling_mat__area
                  if (area == igfs__area) {
                    comment("Collect all suitable ling_mat biomass for igfs")
                    ling_mat__suit_igfs[, ling_mat__area_idx, ling_mat__age_idx] <- 1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_mat__midlen - param[["ling.igfs.l50"]])))
                    ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__suit_igfs[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]
                    igfs__catch[igfs__area_idx] <- igfs__catch[igfs__area_idx] + sum(ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx])
                  }
                }
            }
        }
        {
            igfs__area_idx <- (1)
            ling_imm__area_idx <- (1)
            {
                comment("Scale igfs catch of ling_imm by total expected catch")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  area <- ling_imm__area
                  if (area == igfs__area) {
                    ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx] * (inttypelookup_getdefault(igfs_totaldata__lookup, (area * 1000000L + cur_year * 100L + cur_step), 0)/igfs__catch[igfs__area_idx])
                    ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__totalpredate[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]
                  }
                }
            }
        }
        {
            igfs__area_idx <- (1)
            ling_mat__area_idx <- (1)
            {
                comment("Scale igfs catch of ling_mat by total expected catch")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  area <- ling_mat__area
                  if (area == igfs__area) {
                    ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx] * (inttypelookup_getdefault(igfs_totaldata__lookup, (area * 1000000L + cur_year * 100L + cur_step), 0)/igfs__catch[igfs__area_idx])
                    ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__totalpredate[, ling_mat__area_idx, ling_mat__age_idx] + ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]
                  }
                }
            }
        }
        {
            comment("Temporarily convert to being proportion of totalpredate")
            ling_imm__igfs <- ling_imm__igfs/avoid_zero_vec(ling_imm__totalpredate)
        }
        {
            comment("Temporarily convert to being proportion of totalpredate")
            ling_mat__igfs <- ling_mat__igfs/avoid_zero_vec(ling_mat__totalpredate)
        }
        {
            comment("Calculate ling_imm overconsumption coefficient")
            ling_imm__consratio <- ling_imm__totalpredate/avoid_zero_vec(ling_imm__num * ling_imm__wgt)
            ling_imm__consratio <- 0.96 - logspace_add_vec((0.96 - ling_imm__consratio) * 100, 0.96)/100
            if (TRUE) 
                assert_msg(~all(ling_imm__consratio <= 1), "g3a_predate_fleet: ling_imm__consratio <= 1, can't consume more fish than currently exist")
            comment("Apply overconsumption to prey")
            ling_imm__overconsumption <- sum(ling_imm__totalpredate)
            ling_imm__totalpredate <- (ling_imm__num * ling_imm__wgt) * ling_imm__consratio
            ling_imm__overconsumption <- ling_imm__overconsumption - sum(ling_imm__totalpredate)
            ling_imm__num <- ling_imm__num * (1 - ling_imm__consratio)
        }
        {
            comment("Calculate ling_mat overconsumption coefficient")
            ling_mat__consratio <- ling_mat__totalpredate/avoid_zero_vec(ling_mat__num * ling_mat__wgt)
            ling_mat__consratio <- 0.96 - logspace_add_vec((0.96 - ling_mat__consratio) * 100, 0.96)/100
            if (TRUE) 
                assert_msg(~all(ling_mat__consratio <= 1), "g3a_predate_fleet: ling_mat__consratio <= 1, can't consume more fish than currently exist")
            comment("Apply overconsumption to prey")
            ling_mat__overconsumption <- sum(ling_mat__totalpredate)
            ling_mat__totalpredate <- (ling_mat__num * ling_mat__wgt) * ling_mat__consratio
            ling_mat__overconsumption <- ling_mat__overconsumption - sum(ling_mat__totalpredate)
            ling_mat__num <- ling_mat__num * (1 - ling_mat__consratio)
        }
        {
            comment("Zero igfs catch before working out post-adjustment value")
            igfs__catch[] <- 0
        }
        {
            ling_imm__area_idx <- (1)
            igfs__area_idx <- (1)
            {
                comment("Revert to being total biomass (applying overconsumption in process)")
                ling_imm__igfs <- ling_imm__igfs * ling_imm__totalpredate
                comment("Update total catch")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  area <- ling_imm__area
                  if (area == igfs__area) 
                    igfs__catch[igfs__area_idx] <- igfs__catch[igfs__area_idx] + sum(ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx])
                }
            }
        }
        {
            ling_mat__area_idx <- (1)
            igfs__area_idx <- (1)
            {
                comment("Revert to being total biomass (applying overconsumption in process)")
                ling_mat__igfs <- ling_mat__igfs * ling_mat__totalpredate
                comment("Update total catch")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  area <- ling_mat__area
                  if (area == igfs__area) 
                    igfs__catch[igfs__area_idx] <- igfs__catch[igfs__area_idx] + sum(ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx])
                }
            }
        }
        {
            ling_imm__area_idx <- (1)
            {
                comment("Natural mortality for ling_imm")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  area <- ling_imm__area
                  ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * exp(-(param[["lingimm.M"]]) * cur_step_size)
                }
            }
        }
        {
            ling_mat__area_idx <- (1)
            {
                comment("Natural mortality for ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  area <- ling_mat__area
                  ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] * exp(-(param[["lingmat.M"]]) * cur_step_size)
                }
            }
        }
        if (cur_step_final) {
            comment("Reset transitioning arrays")
            ling_imm__transitioning_num[] <- 0
            ling_imm__transitioning_wgt[] <- ling_imm__wgt[]
        }
        {
            ling_imm__area_idx <- (1)
            {
                comment("g3a_grow for ling_imm")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  area <- ling_imm__area
                  {
                    if (ling_imm__growth_lastcalc != floor(cur_step_size * 12L)) {
                      comment("Calculate length/weight delta matrices for current lengthgroups")
                      ling_imm__growth_l <- growth_bbinom(avoid_zero_vec(avoid_zero_vec((param[["ling.Linf"]] - ling_imm__midlen) * (1 - exp(-((param[["ling.k"]] * 0.001)) * cur_step_size)))/ling_imm__plusdl), 15, avoid_zero((param[["ling.bbin"]] * 10)))
                      ling_imm__growth_w <- (g3a_grow_weightsimple_vec_rotate(pow_vec(ling_imm__midlen, param[["lingimm.wbeta"]]), 15 + 1) - g3a_grow_weightsimple_vec_extrude(pow_vec(ling_imm__midlen, param[["lingimm.wbeta"]]), 15 + 1)) * param[["lingimm.walpha"]]
                      comment("Don't recalculate until cur_step_size changes")
                      ling_imm__growth_lastcalc <- floor(cur_step_size * 12L)
                    }
                    if (TRUE) 
                      ling_imm__prevtotal <- sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])
                    if (cur_step_final) {
                      maturity_ratio <- (1/(1 + exp((0 - (0.001 * param[["ling.mat1"]]) * (ling_imm__midlen - param[["ling.mat2"]])))))
                      {
                        comment("Grow and separate maturing ling_imm")
                        {
                          growthresult <- g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * maturity_ratio, ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                          {
                            ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] <- growthresult[, (1)]
                            ling_imm__transitioning_wgt[, ling_imm__area_idx, ling_imm__age_idx] <- growthresult[, (2)]
                          }
                        }
                        comment("Grow non-maturing ling_imm")
                        {
                          growthresult <- g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] * (1 - maturity_ratio), ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                          {
                            ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- growthresult[, (1)]
                            ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- growthresult[, (2)]
                          }
                        }
                      }
                    }
                    else {
                      comment("Update ling_imm using delta matrices")
                      {
                        growthresult <- g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                        {
                          ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- growthresult[, (1)]
                          ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- growthresult[, (2)]
                        }
                      }
                    }
                    if (TRUE) 
                      if (cur_step_final) 
                        assert_msg(~abs(ling_imm__prevtotal - sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx]) - sum(ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx])) < 1e-04, "g3a_growmature: ling_imm__num totals are not the same before and after growth (excluding maturation)")
                      else assert_msg(~abs(ling_imm__prevtotal - sum(ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx])) < 1e-04, "g3a_growmature: ling_imm__num totals are not the same before and after growth")
                  }
                }
            }
        }
        {
            ling_mat__area_idx <- (1)
            {
                comment("g3a_grow for ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  area <- ling_mat__area
                  {
                    if (ling_mat__growth_lastcalc != floor(cur_step_size * 12L)) {
                      comment("Calculate length/weight delta matrices for current lengthgroups")
                      ling_mat__growth_l <- growth_bbinom(avoid_zero_vec(avoid_zero_vec((param[["ling.Linf"]] - ling_mat__midlen) * (1 - exp(-((param[["ling.k"]] * 0.001)) * cur_step_size)))/ling_mat__plusdl), 15, avoid_zero((param[["ling.bbin"]] * 10)))
                      ling_mat__growth_w <- (g3a_grow_weightsimple_vec_rotate(pow_vec(ling_mat__midlen, param[["lingmat.wbeta"]]), 15 + 1) - g3a_grow_weightsimple_vec_extrude(pow_vec(ling_mat__midlen, param[["lingmat.wbeta"]]), 15 + 1)) * param[["lingmat.walpha"]]
                      comment("Don't recalculate until cur_step_size changes")
                      ling_mat__growth_lastcalc <- floor(cur_step_size * 12L)
                    }
                    if (TRUE) 
                      ling_mat__prevtotal <- sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                    comment("Update ling_mat using delta matrices")
                    {
                      growthresult <- g3a_grow_apply(ling_mat__growth_l, ling_mat__growth_w, ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx], ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx])
                      {
                        ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- growthresult[, (1)]
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- growthresult[, (2)]
                      }
                    }
                    if (TRUE) 
                      assert_msg(~abs(ling_mat__prevtotal - sum(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])) < 1e-04, "g3a_growmature: ling_mat__num totals are not the same before and after growth")
                  }
                }
            }
        }
        {
            ling_imm__area_idx <- (1)
            ling_mat__area_idx <- (1)
            {
                comment("Move ling_imm to ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (age >= ling_imm__minage && age <= ling_imm__maxage) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  {
                    area <- ling_mat__area
                    if (area == ling_imm__area) {
                      ling_imm__age_idx <- age - ling_imm__minage + 1L
                      if (cur_step_final) {
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + ling_imm__transitioning_wgt[, ling_imm__area_idx, ling_imm__age_idx] * ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx]
                        ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx]
                        ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx] - ling_imm__transitioning_num[, ling_imm__area_idx, ling_imm__age_idx]
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/avoid_zero_vec(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                      }
                    }
                  }
                }
                comment("Move any unclaimed stock back to ling_imm")
                if (cur_step_final) 
                  ling_imm__num <- ling_imm__num + ling_imm__transitioning_num
            }
        }
        {
            ling_imm__area_idx <- (1)
            {
                comment("g3a_renewal_normalparam for ling_imm")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if ((cur_step == 1 && age == 5)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  {
                    area <- ling_imm__area
                    {
                      comment("Calculate exp(-(dnorm**2) * 0.5)")
                      ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(((ling_imm__midlen - ((param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))))) * (1/(ling_imm_stddev[[age - 3L + 1L]])))^2) * 0.5)
                      comment("scale results")
                      ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * (10000/sum(ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx]))
                      ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * (param[["ling.rec.scalar"]] * nvl(param[[paste("ling.rec", cur_year, sep = ".")]], stop("Out of range: ", "ling.rec")))
                      comment("Generate corresponding mean weight")
                      ling_imm__renewalwgt[, ling_imm__area_idx, ling_imm__age_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                      comment("Add result to ling_imm")
                      ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ratio_add_vec(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__renewalwgt[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx])
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx]
                    }
                  }
                }
            }
        }
        {
            ling_imm__area_idx <- (1)
            {
                comment("g3a_renewal_normalparam for ling_imm")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if ((cur_step == 1 && age == 3)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  {
                    area <- ling_imm__area
                    {
                      comment("Calculate exp(-(dnorm**2) * 0.5)")
                      ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- exp(-(((ling_imm__midlen - ((param[["ling.Linf"]] * (1 - exp(-1 * (0.001 * param[["ling.k"]]) * (age - (1 + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/(0.001 * param[["ling.k"]])))))))) * (1/(ling_imm_stddev[[age - 3 + 1]])))^2) * 0.5)
                      comment("scale results")
                      ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * (10000/sum(ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx]))
                      ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx] * (param[["ling.rec.scalar"]] * nvl(param[[paste("ling.rec", cur_year, sep = ".")]], stop("Out of range: ", "ling.rec")))
                      comment("Generate corresponding mean weight")
                      ling_imm__renewalwgt[, ling_imm__area_idx, ling_imm__age_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                      comment("Add result to ling_imm")
                      ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx] <- ratio_add_vec(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__renewalwgt[, ling_imm__area_idx, ling_imm__age_idx], ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx])
                      ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] <- ling_imm__num[, ling_imm__area_idx, ling_imm__age_idx] + ling_imm__renewalnum[, ling_imm__area_idx, ling_imm__age_idx]
                    }
                  }
                }
            }
        }
        {
            ling_imm__area_idx <- (1)
            {
                comment("g3l_catchdistribution: Collect catch from igfs/ling_imm for ldist_lln")
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  area <- ling_imm__area
                  {
                    comment("Take prey_stock__fleet_stock weight, convert to individuals, add to our count")
                    cdist_ldist_lln_model__num[] <- cdist_ldist_lln_model__num[] + ling_imm__igfs[, ling_imm__area_idx, ling_imm__age_idx]/avoid_zero_vec(ling_imm__wgt[, ling_imm__area_idx, ling_imm__age_idx])
                  }
                }
            }
        }
        {
            ling_mat__area_idx <- (1)
            {
                comment("g3l_catchdistribution: Collect catch from igfs/ling_mat for ldist_lln")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  area <- ling_mat__area
                  {
                    comment("Take prey_stock__fleet_stock weight, convert to individuals, add to our count")
                    cdist_ldist_lln_model__num[] <- cdist_ldist_lln_model__num[] + ling_mat__igfs[, ling_mat__area_idx, ling_mat__age_idx]/avoid_zero_vec(ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx])
                  }
                }
            }
        }
        {
            comment("g3l_catchdistribution: Compare cdist_ldist_lln_model to cdist_ldist_lln_obs")
            {
                cdist_ldist_lln_obs__time_idx <- intintlookup_getdefault(times_cdist_ldist_lln_obs__lookup, (cur_year * 1000L + cur_step), -1L)
                if (cdist_ldist_lln_obs__time_idx >= (1L)) {
                  cur_cdist_nll <- sum((cdist_ldist_lln_model__num[]/avoid_zero(sum(cdist_ldist_lln_model__num[])) - cdist_ldist_lln_obs__num[, cdist_ldist_lln_obs__time_idx]/avoid_zero(sum(cdist_ldist_lln_obs__num[, cdist_ldist_lln_obs__time_idx])))^2)
                  {
                    nll <- nll + param[["ldist_lln_weight"]] * cur_cdist_nll
                    nll_cdist_ldist_lln__num[cur_time + 1L] <- nll_cdist_ldist_lln__num[cur_time + 1L] + cur_cdist_nll
                    nll_cdist_ldist_lln__weight[cur_time + 1L] <- param[["ldist_lln_weight"]]
                  }
                }
            }
            comment("Zero counters for next reporting period")
            cdist_ldist_lln_model__num[] <- 0
        }
        {
            comment("Reset understocking total")
            g3l_understocking_total <- 0
        }
        {
            comment("g3l_understocking for ling_imm")
            comment("Add understocking from ling_imm as biomass to nll")
            g3l_understocking_total <- g3l_understocking_total + ling_imm__overconsumption
        }
        {
            comment("g3l_understocking for ling_mat")
            comment("Add understocking from ling_mat as biomass to nll")
            g3l_understocking_total <- g3l_understocking_total + ling_mat__overconsumption
        }
        {
            comment("g3l_understocking: Combine and add to nll")
            g3l_understocking_total <- g3l_understocking_total^2
            nll <- nll + g3l_understocking_total
            nll_understocking__wgt[cur_time + 1L] <- nll_understocking__wgt[cur_time + 1L] + g3l_understocking_total
            nll_understocking__weight[cur_time + 1L] <- 1
        }
        if (cur_step_final) {
            comment("g3a_age for ling_imm")
            for (age in seq(ling_imm__maxage, ling_imm__minage, by = -1)) {
                ling_imm__age_idx <- age - ling_imm__minage + 1L
                {
                  comment("Check stock has remained finite for this step")
                  if (TRUE) 
                    assert_msg(~all(is.finite(ling_imm__num[, , ling_imm__age_idx])), "ling_imm__num became NaN/Inf in this timestep")
                  if (TRUE) 
                    assert_msg(~all(is.finite(ling_imm__wgt[, , ling_imm__age_idx])), "ling_imm__wgt became NaN/Inf in this timestep")
                  if (age == ling_imm__maxage) {
                    comment("Move oldest ling_imm into ling_imm_movement")
                    ling_imm_movement__transitioning_num[, , (1)] <- ling_imm__num[, , ling_imm__age_idx]
                    ling_imm_movement__transitioning_wgt[, , (1)] <- ling_imm__wgt[, , ling_imm__age_idx]
                    ling_imm__num[, , ling_imm__age_idx] <- ling_imm__num[, , ling_imm__age_idx - 1L]
                    ling_imm__wgt[, , ling_imm__age_idx] <- ling_imm__wgt[, , ling_imm__age_idx - 1L]
                  }
                  else if (age == ling_imm__minage) {
                    comment("Empty youngest ling_imm age-group")
                    ling_imm__num[, , ling_imm__age_idx] <- 0
                    ling_imm__wgt[, , ling_imm__age_idx] <- 0
                  }
                  else {
                    comment("Move ling_imm age-group to next one up")
                    ling_imm__num[, , ling_imm__age_idx] <- ling_imm__num[, , ling_imm__age_idx - 1L]
                    ling_imm__wgt[, , ling_imm__age_idx] <- ling_imm__wgt[, , ling_imm__age_idx - 1L]
                  }
                }
            }
        }
        if (cur_step_final) {
            comment("g3a_age for ling_mat")
            for (age in seq(ling_mat__maxage, ling_mat__minage, by = -1)) {
                ling_mat__age_idx <- age - ling_mat__minage + 1L
                {
                  comment("Check stock has remained finite for this step")
                  if (TRUE) 
                    assert_msg(~all(is.finite(ling_mat__num[, , ling_mat__age_idx])), "ling_mat__num became NaN/Inf in this timestep")
                  if (TRUE) 
                    assert_msg(~all(is.finite(ling_mat__wgt[, , ling_mat__age_idx])), "ling_mat__wgt became NaN/Inf in this timestep")
                  if (age == ling_mat__maxage) {
                    comment("Oldest ling_mat is a plus-group, combine with younger individuals")
                    ling_mat__wgt[, , ling_mat__age_idx] <- ratio_add_vec(ling_mat__wgt[, , ling_mat__age_idx], ling_mat__num[, , ling_mat__age_idx], ling_mat__wgt[, , ling_mat__age_idx - 1L], ling_mat__num[, , ling_mat__age_idx - 1L])
                    ling_mat__num[, , ling_mat__age_idx] <- ling_mat__num[, , ling_mat__age_idx] + ling_mat__num[, , ling_mat__age_idx - 1L]
                  }
                  else if (age == ling_mat__minage) {
                    comment("Empty youngest ling_mat age-group")
                    ling_mat__num[, , ling_mat__age_idx] <- 0
                    ling_mat__wgt[, , ling_mat__age_idx] <- 0
                  }
                  else {
                    comment("Move ling_mat age-group to next one up")
                    ling_mat__num[, , ling_mat__age_idx] <- ling_mat__num[, , ling_mat__age_idx - 1L]
                    ling_mat__wgt[, , ling_mat__age_idx] <- ling_mat__wgt[, , ling_mat__age_idx - 1L]
                  }
                }
            }
        }
        {
            ling_imm_movement__area_idx <- (1)
            ling_mat__area_idx <- (1)
            {
                comment("Move ling_imm_movement to ling_mat")
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (age >= ling_imm_movement__minage && age <= ling_imm_movement__maxage) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  {
                    area <- ling_mat__area
                    if (area == ling_imm_movement__area) {
                      ling_imm_movement__age_idx <- age - ling_imm_movement__minage + 1L
                      if (cur_step_final) {
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- (ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] * ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx]) + ling_imm_movement__transitioning_wgt[, ling_imm_movement__area_idx, ling_imm_movement__age_idx] * ling_imm_movement__transitioning_num[, ling_imm_movement__area_idx, ling_imm_movement__age_idx]
                        ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx] + ling_imm_movement__transitioning_num[, ling_imm_movement__area_idx, ling_imm_movement__age_idx]
                        ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx] <- ling_mat__wgt[, ling_mat__area_idx, ling_mat__age_idx]/avoid_zero_vec(ling_mat__num[, ling_mat__area_idx, ling_mat__age_idx])
                      }
                    }
                  }
                }
            }
        }
    }
    stop("Should have return()ed somewhere in the loop")
}, class = c("g3_r", "function"), parameter_template = list(ling.Linf = 0, ling.k = 0, ling.recl = 0, lingimm.init.scalar = 0, lingimm.M = 0, ling.init.F = 0, lingimm.init = 0, lingimm.walpha = 0, lingimm.wbeta = 0, lingmat.init.scalar = 0, lingmat.M = 0, lingmat.init = 0, lingmat.walpha = 0, lingmat.wbeta = 0, ling.igfs.alpha = 0, ling.igfs.l50 = 0, ling.bbin = 0, ling.mat1 = 0, ling.mat2 = 0, ling.rec.scalar = 0, ling.rec.1994 = 0, ling.rec.1995 = 0, ling.rec.1996 = 0, ling.rec.1997 = 0, ling.rec.1998 = 0, 
    ling.rec.1999 = 0, ling.rec.2000 = 0, ling.rec.2001 = 0, ling.rec.2002 = 0, ling.rec.2003 = 0, ling.rec.2004 = 0, ling.rec.2005 = 0, ling.rec.2006 = 0, ling.rec.2007 = 0, ling.rec.2008 = 0, ling.rec.2009 = 0, ling.rec.2010 = 0, ling.rec.2011 = 0, ling.rec.2012 = 0, ling.rec.2013 = 0, ling.rec.2014 = 0, ling.rec.2015 = 0, ling.rec.2016 = 0, ling.rec.2017 = 0, ling.rec.2018 = 0, ldist_lln_weight = 1))
