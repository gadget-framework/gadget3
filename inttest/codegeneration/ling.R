structure(function (param) 
{
    stopifnot("retro_years" %in% names(param))
    stopifnot("ling.Linf" %in% names(param))
    stopifnot("ling.K" %in% names(param))
    stopifnot("recage" %in% names(param))
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
    stopifnot("ling.mat1" %in% names(param))
    stopifnot("ling.mat2" %in% names(param))
    stopifnot("ling.bbin" %in% names(param))
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
    stopifnot("cdist_sumofsquares_ldist_lln_weight" %in% names(param))
    assert_msg <- function(expr, message) {
        if (isFALSE(expr)) {
            warning(message)
            return(TRUE)
        }
        return(FALSE)
    }
    as_integer <- as.integer
    normalize_vec <- function(a) {
        a/sum(a)
    }
    REPORT <- function(var) {
        var_name <- as.character(sys.call()[[2]])
        attr(nll, var_name) <<- if (var_name == "nll") 
            as.vector(var)
        else var
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
    nonconform_add <- function(base_ar, extra_ar) {
        base_ar + as.vector(extra_ar)
    }
    avoid_zero_vec <- function(a) {
        (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
    }
    logspace_add_vec <- function(a, b) {
        pmax(a, b) + log1p(exp(pmin(a, b) - pmax(a, b)))
    }
    nonconform_mult <- function(base_ar, extra_ar) {
        base_ar * as.vector(extra_ar)
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
    g3a_grow_vec_rotate <- function(vec, a) {
        out <- vapply(seq_len(a), function(i) vec[i:(i + length(vec) - 1)], numeric(length(vec)))
        out[is.na(out)] <- vec[length(vec)]
        out
    }
    pow_vec <- function(a, b) {
        a^b
    }
    g3a_grow_vec_extrude <- function(vec, a) {
        array(vec, dim = c(length(vec), a))
    }
    g3a_grow_matrix_wgt <- function(delta_w) {
        na <- dim(delta_w)[[1]]
        n <- dim(delta_w)[[2]] - 1
        wgt.matrix <- array(0, c(na, na))
        for (lg in 1:na) {
            if (lg == na) {
                wgt.matrix[lg, lg:na] <- delta_w[lg, 1:(na - lg + 1)]
            }
            else if (lg + n > na) {
                wgt.matrix[lg, lg:na] <- delta_w[lg, 1:(na - lg + 1)]
            }
            else {
                wgt.matrix[lg, lg:(n + lg)] <- delta_w[lg, ]
            }
        }
        return(wgt.matrix)
    }
    g3a_grow_matrix_len <- function(delta_l) {
        na <- dim(delta_l)[[1]]
        n <- dim(delta_l)[[2]] - 1
        growth.matrix <- array(0, c(na, na))
        for (lg in 1:na) {
            if (lg == na) {
                growth.matrix[na, na] <- sum(delta_l[lg, ])
            }
            else if (lg + n > na) {
                growth.matrix[lg, lg:(na - 1)] <- delta_l[lg, 1:(na - lg)]
                growth.matrix[lg, na] <- sum(delta_l[lg, (na - lg + 1):(n + 1)])
            }
            else {
                growth.matrix[lg, lg:(n + lg)] <- delta_l[lg, ]
            }
        }
        return(growth.matrix)
    }
    g3a_grow_apply <- function(growth.matrix, wgt.matrix, input_num, input_wgt) {
        na <- dim(growth.matrix)[[1]]
        avoid_zero_vec <- function(a) {
            (pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))))/1000
        }
        growth.matrix <- growth.matrix * as.vector(input_num)
        wgt.matrix <- growth.matrix * (wgt.matrix + as.vector(input_wgt))
        growth.matrix.sum <- colSums(growth.matrix)
        return(array(c(growth.matrix.sum, colSums(wgt.matrix)/avoid_zero_vec(growth.matrix.sum)), dim = c(na, 2)))
    }
    nvl <- function(...) {
        for (i in seq_len(...length())) if (!is.null(...elt(i))) 
            return(...elt(i))
        return(NULL)
    }
    ratio_add_vec <- function(orig_vec, orig_amount, new_vec, new_amount) {
        (orig_vec * orig_amount + new_vec * new_amount)/avoid_zero_vec(orig_amount + new_amount)
    }
    cur_time <- -1L
    cur_year <- 0L
    start_year <- 1994L
    step_count <- length(step_lengths)
    cur_year_projection <- FALSE
    end_year <- 2018L
    cur_step <- 0L
    cur_step_size <- step_lengths[[1]]/12
    cur_step_final <- FALSE
    ling_imm__area <- 1L
    ling_imm__minage <- 3L
    ling_imm__maxage <- 10L
    ling_imm__num <- array(0, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", "age8", 
        "age9", "age10"), area = "area1"))
    ling_imm__wgt <- array(1, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", "age8", 
        "age9", "age10"), area = "area1"))
    ling_mat__area <- 1L
    ling_mat__minage <- 5L
    ling_mat__maxage <- 15L
    ling_mat__num <- array(0, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", "age10", 
        "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_mat__wgt <- array(1, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", "age10", 
        "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    retro_years <- param[["retro_years"]]
    total_steps <- length(step_lengths) * (end_year - retro_years - start_year + 0L) + length(step_lengths) - 1L
    suit_ling_imm_igfs__report <- array(NA, dim = c(length = 35L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf")))
    suit_ling_mat_igfs__report <- array(NA, dim = c(length = 35L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf")))
    nll_understocking__wgt <- array(0, dim = c(time = as_integer(total_steps + 1L)), dimnames = list(time = attributes(gen_dimnames(param))[["time"]]))
    nll <- 0
    igfs__totalsuit <- array(NA, dim = c(area = 1L), dimnames = list(area = "area1"))
    ling_imm__totalpredate <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    ling_mat__totalpredate <- array(NA, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", 
        "age10", "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_imm_igfs__suit <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    igfs__area <- 1L
    ling_mat_igfs__suit <- array(NA, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", 
        "age10", "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_imm_igfs__cons <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
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
    igfs_totaldata <- intlookup_zip(igfs_totaldata_keys, igfs_totaldata_values)
    ling_mat_igfs__cons <- array(NA, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", 
        "age10", "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_imm__consratio <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    ling_imm__overconsumption <- structure(0, desc = "Total overconsumption of ling_imm")
    ling_imm__consconv <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    ling_mat__consratio <- array(NA, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", 
        "age10", "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_mat__overconsumption <- structure(0, desc = "Total overconsumption of ling_mat")
    ling_mat__consconv <- array(NA, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", 
        "age10", "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_imm__predby_igfs <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    ling_mat__predby_igfs <- array(NA, dim = c(length = 35L, age = 11L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age5", "age6", "age7", "age8", "age9", 
        "age10", "age11", "age12", "age13", "age14", "age15"), area = "area1"))
    ling_imm__transitioning_num <- array(0, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", 
        "age7", "age8", "age9", "age10"), area = "area1"))
    ling_imm__transitioning_wgt <- array(NA, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", 
        "age7", "age8", "age9", "age10"), area = "area1"))
    ling_imm__growth_lastcalc <- -1L
    ling_imm__growth_l <- array(NA, dim = c(length = 35L, delta = 16L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), delta = c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15")))
    ling_imm__plusdl <- 4
    ling_imm__growth_w <- array(NA, dim = c(length = 35L, delta = 16L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), delta = c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15")))
    ling_imm__prevtotal <- 0
    ling_mat__growth_lastcalc <- -1L
    ling_mat__growth_l <- array(NA, dim = c(length = 35L, delta = 16L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), delta = c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15")))
    ling_mat__plusdl <- 4
    ling_mat__growth_w <- array(NA, dim = c(length = 35L, delta = 16L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), delta = c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
        "9", "10", "11", "12", "13", "14", "15")))
    ling_mat__prevtotal <- 0
    ling_imm__renewalnum <- array(0, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    ling_imm__renewalwgt <- array(0, dim = c(length = 35L, age = 8L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = c("age3", "age4", "age5", "age6", "age7", 
        "age8", "age9", "age10"), area = "area1"))
    cdist_sumofsquares_ldist_lln_model__area <- 1L
    cdist_sumofsquares_ldist_lln_model__num <- array(0, dim = c(length = 35L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), area = "1"))
    cdist_sumofsquares_ldist_lln_obs__area <- 1L
    cdist_sumofsquares_ldist_lln_obs__times <- intlookup_zip(cdist_sumofsquares_ldist_lln_obs__times_keys, cdist_sumofsquares_ldist_lln_obs__times_values)
    nll_cdist_sumofsquares_ldist_lln__num <- array(0, dim = c(time = as_integer(total_steps + 1L)), dimnames = list(time = attributes(gen_dimnames(param))[["time"]]))
    nll_cdist_sumofsquares_ldist_lln__weight <- array(0, dim = c(time = as_integer(total_steps + 1L)), dimnames = list(time = attributes(gen_dimnames(param))[["time"]]))
    g3l_understocking_total <- 0
    nll_understocking__weight <- array(0, dim = c(time = as_integer(total_steps + 1L)), dimnames = list(time = attributes(gen_dimnames(param))[["time"]]))
    ling_imm_movement__transitioning_num <- array(NA, dim = c(length = 35L, age = 1L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = "age11", area = "area1"))
    ling_imm_movement__transitioning_wgt <- array(NA, dim = c(length = 35L, age = 1L, area = 1L), dimnames = list(length = c("20:24", "24:28", "28:32", "32:36", "36:40", "40:44", "44:48", "48:52", "52:56", "56:60", "60:64", "64:68", "68:72", "72:76", "76:80", "80:84", "84:88", "88:92", "92:96", "96:100", "100:104", "104:108", "108:112", "112:116", "116:120", "120:124", "124:128", "128:132", "132:136", "136:140", "140:144", "144:148", "148:152", "152:156", "156:Inf"), age = "age11", area = "area1"))
    ling_imm_movement__area <- 1L
    ling_imm_movement__minage <- 11L
    ling_imm_movement__maxage <- 11L
    while (TRUE) {
        {
            comment("g3a_time: Start of time period")
            cur_time <- cur_time + 1L
            if (cur_time == 0 && assert_msg(param[["retro_years"]] >= 0, "retro_years must be >= 0")) 
                return(NaN)
            cur_year <- start_year + (cur_time%/%step_count)
            cur_year_projection <- cur_year > end_year - param[["retro_years"]]
            cur_step <- (cur_time%%step_count) + 1L
            cur_step_size <- step_lengths[[cur_step]]/12
            cur_step_final <- cur_step == step_count
        }
        {
            comment("g3a_initialconditions for ling_imm")
            {
                area <- ling_imm__area
                ling_imm__area_idx <- (1L)
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (cur_time == 0L) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  dnorm <- ((ling_imm__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * param[["ling.K"]] * ((age - cur_step_size) - (param[["recage"]] + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/param[["ling.K"]]))))))/ling_imm_stddev[[as_integer((age - cur_step_size)) - 3 + 2]])
                  factor <- (param[["lingimm.init.scalar"]] * exp(-1 * (param[["lingimm.M"]] + param[["ling.init.F"]]) * age) * param[["lingimm.init"]][[as_integer(age) - 3 + 1]])
                  {
                    ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- normalize_vec(exp(-(dnorm^2) * 0.5)) * 10000 * factor
                    ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                  }
                }
            }
        }
        {
            comment("g3a_initialconditions for ling_mat")
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (cur_time == 0L) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  dnorm <- ((ling_mat__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * param[["ling.K"]] * ((age - cur_step_size) - (param[["recage"]] + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/param[["ling.K"]]))))))/ling_mat_stddev[[as_integer((age - cur_step_size)) - 5 + 2]])
                  factor <- (param[["lingmat.init.scalar"]] * exp(-1 * (param[["lingmat.M"]] + param[["ling.init.F"]]) * age) * param[["lingmat.init"]][[as_integer(age) - 5 + 1]])
                  {
                    ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- normalize_vec(exp(-(dnorm^2) * 0.5)) * 10000 * factor
                    ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- param[["lingmat.walpha"]] * ling_mat__midlen^param[["lingmat.wbeta"]]
                  }
                }
            }
        }
        if (reporting_enabled > 0L && cur_time > total_steps) {
            suit_ling_imm_igfs__report[] <- 1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_imm__midlen - param[["ling.igfs.l50"]])))
            REPORT(suit_ling_imm_igfs__report)
        }
        if (reporting_enabled > 0L && cur_time > total_steps) {
            suit_ling_mat_igfs__report[] <- 1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_mat__midlen - param[["ling.igfs.l50"]])))
            REPORT(suit_ling_mat_igfs__report)
        }
        if (reporting_enabled > 0L && cur_time > total_steps) 
            REPORT(nll_understocking__wgt)
        {
            if (cur_time > total_steps) 
                return(nll)
        }
        igfs__totalsuit[] <- 0
        ling_imm__totalpredate[] <- 0
        ling_mat__totalpredate[] <- 0
        {
            comment("g3a_predate_fleet for ling_imm")
            comment("Zero igfs-ling_imm biomass-consuming counter")
            ling_imm_igfs__suit[] <- 0
            {
                area <- ling_imm__area
                ling_imm__area_idx <- (1L)
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (area == igfs__area) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  igfs__area_idx <- (1L)
                  predator_area <- area
                  {
                    comment("Collect all suitable ling_imm biomass for igfs")
                    ling_imm_igfs__suit[, ling_imm__age_idx, ling_imm__area_idx] <- (1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_imm__midlen - param[["ling.igfs.l50"]])))) * ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] * ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx]
                    igfs__totalsuit[igfs__area_idx] <- igfs__totalsuit[igfs__area_idx] + sum(ling_imm_igfs__suit[, ling_imm__age_idx, ling_imm__area_idx])
                  }
                }
            }
        }
        {
            comment("g3a_predate_fleet for ling_mat")
            comment("Zero igfs-ling_mat biomass-consuming counter")
            ling_mat_igfs__suit[] <- 0
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (area == igfs__area) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  igfs__area_idx <- (1L)
                  predator_area <- area
                  {
                    comment("Collect all suitable ling_mat biomass for igfs")
                    ling_mat_igfs__suit[, ling_mat__age_idx, ling_mat__area_idx] <- (1/(1 + exp(-param[["ling.igfs.alpha"]] * (ling_mat__midlen - param[["ling.igfs.l50"]])))) * ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] * ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx]
                    igfs__totalsuit[igfs__area_idx] <- igfs__totalsuit[igfs__area_idx] + sum(ling_mat_igfs__suit[, ling_mat__age_idx, ling_mat__area_idx])
                  }
                }
            }
        }
        {
            comment("Scale igfs catch of ling_imm by total expected catch")
            ling_imm_igfs__cons[] <- 0
            {
                area <- ling_imm__area
                ling_imm__area_idx <- (1L)
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (area == igfs__area) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  igfs__area_idx <- (1L)
                  predator_area <- area
                  total_predsuit <- igfs__totalsuit[igfs__area_idx]
                  ling_imm_igfs__cons[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm_igfs__suit[, ling_imm__age_idx, ling_imm__area_idx] * ((if (area != 1) 
                    0
                  else intlookup_getdefault(igfs_totaldata, (cur_year * 100L + cur_step), 0))/total_predsuit)
                }
            }
            ling_imm__totalpredate[] <- nonconform_add(ling_imm__totalpredate[], ling_imm_igfs__cons[, , ])
        }
        {
            comment("Scale igfs catch of ling_mat by total expected catch")
            ling_mat_igfs__cons[] <- 0
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (area == igfs__area) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  igfs__area_idx <- (1L)
                  predator_area <- area
                  total_predsuit <- igfs__totalsuit[igfs__area_idx]
                  ling_mat_igfs__cons[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat_igfs__suit[, ling_mat__age_idx, ling_mat__area_idx] * ((if (area != 1) 
                    0
                  else intlookup_getdefault(igfs_totaldata, (cur_year * 100L + cur_step), 0))/total_predsuit)
                }
            }
            ling_mat__totalpredate[] <- nonconform_add(ling_mat__totalpredate[], ling_mat_igfs__cons[, , ])
        }
        {
            comment("Calculate ling_imm overconsumption coefficient")
            comment("Apply overconsumption to ling_imm")
            ling_imm__consratio <- ling_imm__totalpredate/avoid_zero_vec(ling_imm__num * ling_imm__wgt)
            ling_imm__consratio <- logspace_add_vec(ling_imm__consratio * -1000, 0.95 * -1000)/-1000
            ling_imm__overconsumption <- sum(ling_imm__totalpredate)
            ling_imm__consconv <- 1/avoid_zero_vec(ling_imm__totalpredate)
            ling_imm__totalpredate <- (ling_imm__num * ling_imm__wgt) * ling_imm__consratio
            ling_imm__overconsumption <- ling_imm__overconsumption - sum(ling_imm__totalpredate)
            ling_imm__consconv <- ling_imm__consconv * ling_imm__totalpredate
            ling_imm__num <- ling_imm__num * (1 - ling_imm__consratio)
        }
        {
            comment("Calculate ling_mat overconsumption coefficient")
            comment("Apply overconsumption to ling_mat")
            ling_mat__consratio <- ling_mat__totalpredate/avoid_zero_vec(ling_mat__num * ling_mat__wgt)
            ling_mat__consratio <- logspace_add_vec(ling_mat__consratio * -1000, 0.95 * -1000)/-1000
            ling_mat__overconsumption <- sum(ling_mat__totalpredate)
            ling_mat__consconv <- 1/avoid_zero_vec(ling_mat__totalpredate)
            ling_mat__totalpredate <- (ling_mat__num * ling_mat__wgt) * ling_mat__consratio
            ling_mat__overconsumption <- ling_mat__overconsumption - sum(ling_mat__totalpredate)
            ling_mat__consconv <- ling_mat__consconv * ling_mat__totalpredate
            ling_mat__num <- ling_mat__num * (1 - ling_mat__consratio)
        }
        {
            comment("Apply overconsumption to ling_imm_igfs__cons")
            ling_imm_igfs__cons <- nonconform_mult(ling_imm_igfs__cons, ling_imm__consconv)
        }
        {
            comment("Apply overconsumption to ling_mat_igfs__cons")
            ling_mat_igfs__cons <- nonconform_mult(ling_mat_igfs__cons, ling_mat__consconv)
        }
        {
            ling_imm__predby_igfs[] <- 0
            ling_imm__predby_igfs[] <- nonconform_add(ling_imm__predby_igfs, ling_imm_igfs__cons[, , ])
        }
        {
            ling_mat__predby_igfs[] <- 0
            ling_mat__predby_igfs[] <- nonconform_add(ling_mat__predby_igfs, ling_mat_igfs__cons[, , ])
        }
        {
            comment("Natural mortality for ling_imm")
            {
                area <- ling_imm__area
                ling_imm__area_idx <- (1L)
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] * exp(-(param[["lingimm.M"]]) * cur_step_size)
                }
            }
        }
        {
            comment("Natural mortality for ling_mat")
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] * exp(-(param[["lingmat.M"]]) * cur_step_size)
                }
            }
        }
        if (cur_step_final) {
            comment("Reset transitioning arrays")
            ling_imm__transitioning_num[] <- 0
            ling_imm__transitioning_wgt[] <- ling_imm__wgt[]
        }
        {
            maturity_ratio <- (1/(1 + exp((0 - (0.001 * param[["ling.mat1"]]) * (ling_imm__midlen - param[["ling.mat2"]])))))
            growth_delta_l <- if (ling_imm__growth_lastcalc == floor(cur_step_size * 12L)) 
                ling_imm__growth_l
            else (ling_imm__growth_l[] <- growth_bbinom(avoid_zero_vec(avoid_zero_vec((param[["ling.Linf"]] - ling_imm__midlen) * (1 - exp(-((param[["ling.K"]] * 0.001)) * cur_step_size)))/ling_imm__plusdl), 15L, avoid_zero((param[["ling.bbin"]] * 10))))
            growth_delta_w <- if (ling_imm__growth_lastcalc == floor(cur_step_size * 12L)) 
                ling_imm__growth_w
            else (ling_imm__growth_w[] <- (g3a_grow_vec_rotate(pow_vec(ling_imm__midlen, param[["lingimm.wbeta"]]), 15L + 1) - g3a_grow_vec_extrude(pow_vec(ling_imm__midlen, param[["lingimm.wbeta"]]), 15L + 1)) * param[["lingimm.walpha"]])
            growthmat_w <- g3a_grow_matrix_wgt(growth_delta_w)
            growthmat_l <- g3a_grow_matrix_len(growth_delta_l)
            {
                comment("g3a_grow for ling_imm")
                {
                  area <- ling_imm__area
                  ling_imm__area_idx <- (1L)
                  for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) {
                    ling_imm__age_idx <- age - ling_imm__minage + 1L
                    growthmatresult <- g3a_grow_apply(growthmat_l, growthmat_w, ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] * maturity_ratio, ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx])
                    growthimmresult <- g3a_grow_apply(growthmat_l, growthmat_w, ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] * (1 - maturity_ratio), ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx])
                    growthresult <- g3a_grow_apply(growthmat_l, growthmat_w, ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx])
                    {
                      if (TRUE) 
                        ling_imm__prevtotal <- sum(ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx])
                      if (cur_step_final) {
                        comment("Grow and separate maturing ling_imm")
                        ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx] <- growthmatresult[, (1)]
                        ling_imm__transitioning_wgt[, ling_imm__age_idx, ling_imm__area_idx] <- growthmatresult[, (2)]
                        comment("Grow non-maturing ling_imm")
                        ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- growthimmresult[, (1)]
                        ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- growthimmresult[, (2)]
                      }
                      else {
                        comment("Update ling_imm using delta matrices")
                        ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- growthresult[, (1)]
                        ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- growthresult[, (2)]
                      }
                      if (TRUE) 
                        if (cur_step_final) 
                          assert_msg(~abs(ling_imm__prevtotal - sum(ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx]) - sum(ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx])) < 1e-04, "g3a_growmature: ling_imm__num totals are not the same before and after growth (excluding maturation)")
                        else assert_msg(~abs(ling_imm__prevtotal - sum(ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx])) < 1e-04, "g3a_growmature: ling_imm__num totals are not the same before and after growth")
                    }
                  }
                }
                ling_imm__growth_lastcalc <- floor(cur_step_size * 12L)
            }
        }
        {
            growth_delta_l <- if (ling_mat__growth_lastcalc == floor(cur_step_size * 12L)) 
                ling_mat__growth_l
            else (ling_mat__growth_l[] <- growth_bbinom(avoid_zero_vec(avoid_zero_vec((param[["ling.Linf"]] - ling_mat__midlen) * (1 - exp(-((param[["ling.K"]] * 0.001)) * cur_step_size)))/ling_mat__plusdl), 15L, avoid_zero((param[["ling.bbin"]] * 10))))
            growth_delta_w <- if (ling_mat__growth_lastcalc == floor(cur_step_size * 12L)) 
                ling_mat__growth_w
            else (ling_mat__growth_w[] <- (g3a_grow_vec_rotate(pow_vec(ling_mat__midlen, param[["lingmat.wbeta"]]), 15L + 1) - g3a_grow_vec_extrude(pow_vec(ling_mat__midlen, param[["lingmat.wbeta"]]), 15L + 1)) * param[["lingmat.walpha"]])
            growthmat_w <- g3a_grow_matrix_wgt(growth_delta_w)
            growthmat_l <- g3a_grow_matrix_len(growth_delta_l)
            {
                comment("g3a_grow for ling_mat")
                {
                  area <- ling_mat__area
                  ling_mat__area_idx <- (1L)
                  for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) {
                    ling_mat__age_idx <- age - ling_mat__minage + 1L
                    growthresult <- g3a_grow_apply(growthmat_l, growthmat_w, ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx], ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx])
                    {
                      if (TRUE) 
                        ling_mat__prevtotal <- sum(ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx])
                      comment("Update ling_mat using delta matrices")
                      ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- growthresult[, (1)]
                      ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- growthresult[, (2)]
                      if (TRUE) 
                        assert_msg(~abs(ling_mat__prevtotal - sum(ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx])) < 1e-04, "g3a_growmature: ling_mat__num totals are not the same before and after growth")
                    }
                  }
                }
                ling_mat__growth_lastcalc <- floor(cur_step_size * 12L)
            }
        }
        {
            comment("Move ling_imm to ling_mat")
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (area == ling_imm__area) 
                  if (age >= ling_imm__minage && age <= ling_imm__maxage) 
                    if (cur_step_final) {
                      ling_mat__age_idx <- age - ling_mat__minage + 1L
                      ling_imm__area_idx <- (1L)
                      {
                        ling_imm__age_idx <- age - ling_imm__minage + 1L
                        {
                          ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- (ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] * ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx]) + ling_imm__transitioning_wgt[, ling_imm__age_idx, ling_imm__area_idx] * ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx]
                          ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] + ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx]
                          ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx] - ling_imm__transitioning_num[, ling_imm__age_idx, ling_imm__area_idx]
                          ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx]/avoid_zero_vec(ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx])
                        }
                      }
                    }
            }
            comment("Move any unclaimed stock back to ling_imm")
            if (cur_step_final) 
                ling_imm__num <- ling_imm__num + ling_imm__transitioning_num
        }
        {
            factor <- (param[["ling.rec.scalar"]] * nvl(param[[paste("ling.rec", cur_year, sep = ".")]], {
                warning("No value found in g3_param_table ling.rec, ifmissing not specified")
                NaN
            }))
            {
                comment("g3a_renewal for ling_imm")
                {
                  area <- ling_imm__area
                  ling_imm__area_idx <- (1L)
                  for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (cur_step == 1 && age == 3) {
                    ling_imm__age_idx <- age - ling_imm__minage + 1L
                    dnorm <- ((ling_imm__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * param[["ling.K"]] * (age - (param[["recage"]] + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/param[["ling.K"]]))))))/ling_imm_stddev[[as_integer(age) - 3 + 1]])
                    {
                      ling_imm__renewalnum[, ling_imm__age_idx, ling_imm__area_idx] <- normalize_vec(exp(-(dnorm^2) * 0.5)) * 10000 * factor
                      ling_imm__renewalwgt[, ling_imm__age_idx, ling_imm__area_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                      comment("Add result to ling_imm")
                      ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- ratio_add_vec(ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__renewalwgt[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__renewalnum[, ling_imm__age_idx, ling_imm__area_idx])
                      ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] + ling_imm__renewalnum[, ling_imm__age_idx, ling_imm__area_idx]
                    }
                  }
                }
            }
        }
        {
            factor <- (param[["ling.rec.scalar"]] * nvl(param[[paste("ling.rec", cur_year, sep = ".")]], {
                warning("No value found in g3_param_table ling.rec, ifmissing not specified")
                NaN
            }))
            {
                comment("g3a_renewal for ling_imm")
                {
                  area <- ling_imm__area
                  ling_imm__area_idx <- (1L)
                  for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (cur_step == 1 && age == 5) {
                    ling_imm__age_idx <- age - ling_imm__minage + 1L
                    dnorm <- ((ling_imm__midlen - (param[["ling.Linf"]] * (1 - exp(-1 * param[["ling.K"]] * (age - (param[["recage"]] + log(1 - param[["ling.recl"]]/param[["ling.Linf"]])/param[["ling.K"]]))))))/ling_imm_stddev[[as_integer(age) - 3L + 1L]])
                    {
                      ling_imm__renewalnum[, ling_imm__age_idx, ling_imm__area_idx] <- normalize_vec(exp(-(dnorm^2) * 0.5)) * 10000 * factor
                      ling_imm__renewalwgt[, ling_imm__age_idx, ling_imm__area_idx] <- param[["lingimm.walpha"]] * ling_imm__midlen^param[["lingimm.wbeta"]]
                      comment("Add result to ling_imm")
                      ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- ratio_add_vec(ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__renewalwgt[, ling_imm__age_idx, ling_imm__area_idx], ling_imm__renewalnum[, ling_imm__age_idx, ling_imm__area_idx])
                      ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] + ling_imm__renewalnum[, ling_imm__age_idx, ling_imm__area_idx]
                    }
                  }
                }
            }
        }
        {
            comment("g3l_catchdistribution_sumofsquares: Collect catch from igfs/ling_imm for cdist_sumofsquares_ldist_lln")
            {
                area <- ling_imm__area
                ling_imm__area_idx <- (1L)
                for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) if (area == cdist_sumofsquares_ldist_lln_model__area) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  cdist_sumofsquares_ldist_lln_model__area_idx <- (1L)
                  {
                    comment("Take predprey__cons weight, convert to individuals, add to our count")
                    cdist_sumofsquares_ldist_lln_model__num[, cdist_sumofsquares_ldist_lln_model__area_idx] <- cdist_sumofsquares_ldist_lln_model__num[, cdist_sumofsquares_ldist_lln_model__area_idx] + (ling_imm_igfs__cons[, ling_imm__age_idx, ling_imm__area_idx]/avoid_zero_vec(ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx]))
                  }
                }
            }
        }
        {
            comment("g3l_catchdistribution_sumofsquares: Collect catch from igfs/ling_mat for cdist_sumofsquares_ldist_lln")
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (area == cdist_sumofsquares_ldist_lln_model__area) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  cdist_sumofsquares_ldist_lln_model__area_idx <- (1L)
                  {
                    comment("Take predprey__cons weight, convert to individuals, add to our count")
                    cdist_sumofsquares_ldist_lln_model__num[, cdist_sumofsquares_ldist_lln_model__area_idx] <- cdist_sumofsquares_ldist_lln_model__num[, cdist_sumofsquares_ldist_lln_model__area_idx] + (ling_mat_igfs__cons[, ling_mat__age_idx, ling_mat__area_idx]/avoid_zero_vec(ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx]))
                  }
                }
            }
        }
        {
            comment("g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_ldist_lln_model to cdist_sumofsquares_ldist_lln_obs")
            {
                area <- cdist_sumofsquares_ldist_lln_model__area
                cdist_sumofsquares_ldist_lln_model__area_idx <- (1L)
                if (area == cdist_sumofsquares_ldist_lln_obs__area) {
                  cdist_sumofsquares_ldist_lln_model__sstotal <- avoid_zero(sum(cdist_sumofsquares_ldist_lln_model__num[, cdist_sumofsquares_ldist_lln_model__area_idx]))
                  cdist_sumofsquares_ldist_lln_obs__area_idx <- (1L)
                  cdist_sumofsquares_ldist_lln_obs__time_idx <- intlookup_getdefault(cdist_sumofsquares_ldist_lln_obs__times, (cur_year * 100L + cur_step), -1L)
                  if (cdist_sumofsquares_ldist_lln_obs__time_idx >= (1L)) {
                    cdist_sumofsquares_ldist_lln_obs__sstotal <- avoid_zero(sum(cdist_sumofsquares_ldist_lln_obs__num[, cdist_sumofsquares_ldist_lln_obs__time_idx, cdist_sumofsquares_ldist_lln_obs__area_idx]))
                    cur_cdist_nll <- sum((((cdist_sumofsquares_ldist_lln_model__num[, cdist_sumofsquares_ldist_lln_model__area_idx]/cdist_sumofsquares_ldist_lln_model__sstotal) - (cdist_sumofsquares_ldist_lln_obs__num[, cdist_sumofsquares_ldist_lln_obs__time_idx, cdist_sumofsquares_ldist_lln_obs__area_idx]/cdist_sumofsquares_ldist_lln_obs__sstotal))^2))
                    {
                      nll <- nll + param[["cdist_sumofsquares_ldist_lln_weight"]] * cur_cdist_nll
                      nll_cdist_sumofsquares_ldist_lln__num[cur_time + 1L] <- nll_cdist_sumofsquares_ldist_lln__num[cur_time + 1L] + cur_cdist_nll
                      nll_cdist_sumofsquares_ldist_lln__weight[cur_time + 1L] <- param[["cdist_sumofsquares_ldist_lln_weight"]]
                    }
                  }
                }
            }
            comment("Zero counters for next reporting period")
            cdist_sumofsquares_ldist_lln_model__num[] <- 0
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
            nll <- nll + 1e+08 * g3l_understocking_total
            nll_understocking__wgt[cur_time + 1L] <- nll_understocking__wgt[cur_time + 1L] + g3l_understocking_total
            nll_understocking__weight[cur_time + 1L] <- 1e+08
        }
        if (cur_step_final) {
            ling_imm_movement__area_idx <- (1)
            ling_imm__area_idx <- (1)
            {
                comment("g3a_age for ling_imm")
                for (age in seq(ling_imm__maxage, ling_imm__minage, by = -1)) {
                  ling_imm__age_idx <- age - ling_imm__minage + 1L
                  {
                    comment("Check stock has remained finite for this step")
                    if (age == ling_imm__maxage) {
                      comment("Move oldest ling_imm into ling_imm_movement")
                      ling_imm_movement__transitioning_num[, (1), ling_imm_movement__area_idx] <- ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx]
                      ling_imm_movement__transitioning_wgt[, (1), ling_imm_movement__area_idx] <- ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx]
                      ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__num[, ling_imm__age_idx - 1, ling_imm__area_idx]
                      ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__wgt[, ling_imm__age_idx - 1, ling_imm__area_idx]
                    }
                    else if (age == ling_imm__minage) {
                      comment("Empty youngest ling_imm age-group")
                      ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- 0
                    }
                    else {
                      comment("Move ling_imm age-group to next one up")
                      ling_imm__num[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__num[, ling_imm__age_idx - 1, ling_imm__area_idx]
                      ling_imm__wgt[, ling_imm__age_idx, ling_imm__area_idx] <- ling_imm__wgt[, ling_imm__age_idx - 1, ling_imm__area_idx]
                    }
                  }
                }
            }
        }
        if (cur_step_final) {
            ling_mat__area_idx <- (1)
            {
                comment("g3a_age for ling_mat")
                for (age in seq(ling_mat__maxage, ling_mat__minage, by = -1)) {
                  ling_mat__age_idx <- age - ling_mat__minage + 1L
                  {
                    comment("Check stock has remained finite for this step")
                    if (age == ling_mat__maxage) {
                      comment("Oldest ling_mat is a plus-group, combine with younger individuals")
                      ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- ratio_add_vec(ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx], ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx], ling_mat__wgt[, ling_mat__age_idx - 1, ling_mat__area_idx], ling_mat__num[, ling_mat__age_idx - 1, ling_mat__area_idx])
                      ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] + ling_mat__num[, ling_mat__age_idx - 1, ling_mat__area_idx]
                    }
                    else if (age == ling_mat__minage) {
                      comment("Empty youngest ling_mat age-group")
                      ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- 0
                    }
                    else {
                      comment("Move ling_mat age-group to next one up")
                      ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__num[, ling_mat__age_idx - 1, ling_mat__area_idx]
                      ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__wgt[, ling_mat__age_idx - 1, ling_mat__area_idx]
                    }
                  }
                }
            }
        }
        {
            comment("Move ling_imm_movement to ling_mat")
            {
                area <- ling_mat__area
                ling_mat__area_idx <- (1L)
                for (age in seq(ling_mat__minage, ling_mat__maxage, by = 1)) if (area == ling_imm_movement__area) 
                  if (age >= ling_imm_movement__minage && age <= ling_imm_movement__maxage) 
                    if (cur_step_final) {
                      ling_mat__age_idx <- age - ling_mat__minage + 1L
                      ling_imm_movement__area_idx <- (1L)
                      {
                        ling_imm_movement__age_idx <- age - ling_imm_movement__minage + 1L
                        {
                          ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- (ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] * ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx]) + ling_imm_movement__transitioning_wgt[, ling_imm_movement__age_idx, ling_imm_movement__area_idx] * ling_imm_movement__transitioning_num[, ling_imm_movement__age_idx, ling_imm_movement__area_idx]
                          ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx] + ling_imm_movement__transitioning_num[, ling_imm_movement__age_idx, ling_imm_movement__area_idx]
                          ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx] <- ling_mat__wgt[, ling_mat__age_idx, ling_mat__area_idx]/avoid_zero_vec(ling_mat__num[, ling_mat__age_idx, ling_mat__area_idx])
                        }
                      }
                    }
            }
        }
    }
}, class = c("g3_r", "function"), parameter_template = list(retro_years = 0, ling.Linf = 1, ling.K = 1, recage = 0, ling.recl = 0, lingimm.init.scalar = 0, lingimm.M = 0, ling.init.F = 0, lingimm.init = 0, lingimm.walpha = 0, lingimm.wbeta = 0, lingmat.init.scalar = 0, lingmat.M = 0, lingmat.init = 0, lingmat.walpha = 0, lingmat.wbeta = 0, ling.igfs.alpha = 0, ling.igfs.l50 = 0, ling.mat1 = 0, ling.mat2 = 0, ling.bbin = 0, ling.rec.scalar = 0, ling.rec.1994 = 0, ling.rec.1995 = 0, ling.rec.1996 = 0, 
    ling.rec.1997 = 0, ling.rec.1998 = 0, ling.rec.1999 = 0, ling.rec.2000 = 0, ling.rec.2001 = 0, ling.rec.2002 = 0, ling.rec.2003 = 0, ling.rec.2004 = 0, ling.rec.2005 = 0, ling.rec.2006 = 0, ling.rec.2007 = 0, ling.rec.2008 = 0, ling.rec.2009 = 0, ling.rec.2010 = 0, ling.rec.2011 = 0, ling.rec.2012 = 0, ling.rec.2013 = 0, ling.rec.2014 = 0, ling.rec.2015 = 0, ling.rec.2016 = 0, ling.rec.2017 = 0, ling.rec.2018 = 0, cdist_sumofsquares_ldist_lln_weight = 1))
