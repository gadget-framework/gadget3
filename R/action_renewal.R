# Generate function saying whether this lengthgroup should be renewed into
renewal_into <- function (stock) {
    if (!is.null(stock$dim$tag)) return(~stock__tag_idx == stock__untagged_idx)
    return(TRUE)
}

# Parameterised vonb formula, for use as mean_f
g3a_renewal_vonb_recl <- function(
        Linf = g3_parameterized('Linf', value = 1, by_stock = by_stock),
        K = g3_parameterized('K', value = 1, by_stock = by_stock),
        recl = g3_parameterized('recl', by_stock = by_stock),
        recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
        by_stock = TRUE) {
    f_substitute(
        quote( Linf * (1 - exp(-1 * K * (age - (recage + log(1 - recl/Linf)/K) ))) ),
        list(
            Linf = Linf,
            K = K,
            recl = recl,
            recage = recage))
}
g3a_renewal_vonb_t0 <- function(
        Linf = g3_parameterized('Linf', value = 1, by_stock = by_stock),
        K = g3_parameterized('K', value = 1, by_stock = by_stock),
        t0 = g3_parameterized('t0', by_stock = by_stock),
        by_stock = TRUE) {
    f_substitute(
        quote( Linf * (1 - exp(-1 * K * (age - t0))) ),
        list(
            Linf = Linf,
            K = K,
            t0 = t0))
}
g3a_renewal_vonb <- g3a_renewal_vonb_recl  # NB: Default to _recl for backwards-compatibility

g3a_renewal_initabund <- function(
    scalar = g3_parameterized('init.scalar', value = 1, by_stock = by_stock),
    init = g3_parameterized('init', value = 1, by_stock = by_stock, by_age = TRUE),
    M = g3_parameterized('M', by_stock = by_stock, by_age = TRUE),
    init_F = g3_parameterized('init.F', by_stock = by_stock_f),
    recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
    proportion_f = ~1,
    by_stock = TRUE,
    by_stock_f = FALSE){
  f_substitute(
    ~scalar * init * exp(-1 * (M + init_F) * (age - recage)) * proportion_f,
    list(scalar = scalar,
         init = init,
         M = M,
         init_F = init_F,
         recage = recage,
         proportion_f = proportion_f)
  )
}

g3a_renewal_len_dnorm <- function(
        mean_f,
        stddev_f = g3_parameterized('init.sd', value = 10,
            by_stock = by_stock, by_age = by_age),
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        by_stock = TRUE,
        by_age = FALSE) {
    g3_formula(
        quote( normalize_vec(ren_dnorm) * 10000 * factor ),
        ren_dnorm = f_substitute(
            # NB: sd must be > 0
            quote(dnorm(stock__midlen, mean_f, avoid_zero(stddev_f))),
            list(mean_f = mean_f, stddev_f = stddev_f) ),
        factor = factor_f)
}

g3a_renewal_wgt_wl <- function(
        alpha_f = g3_parameterized('walpha', by_stock = by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = by_stock),
        by_stock = TRUE) {
    f_substitute(quote(
        alpha_f * stock__midlen**beta_f
    ), list(
        alpha_f = alpha_f,
        beta_f = beta_f))
}

# Assign number / mean weight based on formulae
g3a_initialconditions <- function (stock, num_f, wgt_f, run_f = ~cur_time == 0L, run_at = g3_action_order$initial) {
    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)

    out <- list()
    action_name <- unique_action_name()
    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_initialconditions for ", stock)
        stock_iterate(stock, if (run_f && renew_into_f) {
            stock_ss(stock__num) <- num_f
            stock_ss(stock__wgt) <- wgt_f
        })
    }, list(
        num_f = num_f,
        wgt_f = wgt_f,
        renew_into_f = renewal_into(stock),
        run_f = run_f)))
    return(out)
}

# Steps to set up renewal of stocks on first step
g3a_initialconditions_normalparam <- function (
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        stddev_f = g3_parameterized('init.sd', value = 10,
            by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        age_offset = quote( cur_step_size ),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~cur_time == 0L,
        run_at = g3_action_order$initial) {

    # Replace "age" with "age - cur_step_size", i.e. pretending this is happening at time "-1"
    if (!is.null(age_offset)) {
        age_offset <- f_substitute(quote(age - age_offset), list(age_offset = age_offset))
        mean_f <- f_substitute(mean_f, list(age = age_offset))
        stddev_f <- f_substitute(stddev_f, list(age = age_offset))
    }

    # NB: Generate action name with our arguments
    out <- list()
    action_name <- unique_action_name()
    out[[step_id(run_at, stock, action_name)]] <- g3a_initialconditions(
        stock,
        num_f = g3a_renewal_len_dnorm(mean_f, stddev_f, factor_f),
        wgt_f = g3a_renewal_wgt_wl(alpha_f, beta_f),
        run_f = run_f,
        run_at = run_at)[[1]]
    return(out)
}

# normalparam, but with a cv_f instead of stddev_f
g3a_initialconditions_normalcv <- function (
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        cv_f = g3_parameterized('lencv', by_stock = by_stock, value = 0.1,
            optimise = FALSE),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        age_offset = quote( cur_step_size ),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~cur_time == 0L,
        run_at = g3_action_order$initial) {
    g3a_initialconditions_normalparam(
        stock = stock,
        factor_f = factor_f,
        mean_f = mean_f,
        stddev_f = f_substitute(quote(mean_f * cv_f), list(mean_f = mean_f, cv_f = cv_f)),
        alpha_f = alpha_f,
        beta_f = beta_f,
        age_offset = age_offset,
        run_f = run_f,
        run_at = run_at)
}

# Assign number / mean weight based on formulae
g3a_renewal <- function (stock, num_f, wgt_f, run_f = ~TRUE, run_at = g3_action_order$renewal) {
    # See InitialCond::Initialise
    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)
    stock__renewalnum <- g3_stock_instance(stock, 0)
    stock__renewalwgt <- g3_stock_instance(stock, 0)

    out <- list()
    action_name <- unique_action_name()
    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_renewal for ", stock)
        stock_iterate(stock, if (run_f && renew_into_f) {
            stock_ss(stock__renewalnum) <- num_f
            stock_ss(stock__renewalwgt) <- wgt_f

            debug_trace("Add result to ", stock)
            stock_ss(stock__wgt) <- ratio_add_vec(
                stock_ss(stock__wgt), stock_ss(stock__num),
                stock_ss(stock__renewalwgt), stock_ss(stock__renewalnum))
            stock_ss(stock__num) <- stock_ss(stock__num) + stock_ss(stock__renewalnum)
        })
    }, list(
        num_f = num_f, wgt_f = wgt_f,
        renew_into_f = renewal_into(stock),
        run_f = run_f)))
    return(out)
}

# Steps to set up renewal of stocks on any stock
g3a_renewal_normalparam <- function (
        stock,
        factor_f = g3_parameterized('rec',
            by_stock = by_stock,
            by_year = TRUE,
            scale = g3_parameterized(
                name = 'rec.scalar',
                by_stock = by_stock),
            ifmissing = NaN),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        stddev_f = g3_parameterized('rec.sd', value = 10, by_stock = by_stock),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        wgt_by_stock = TRUE,
        run_age = quote(stock__minage),
        run_projection = FALSE,
        run_step = 1,
        run_f = NULL,
        run_at = g3_action_order$renewal) {

    if (is.null(run_f)) run_f <- f_substitute(quote( age && step && proj ), list(
        age = (if (is.null(run_age)) TRUE else f_substitute(quote(age == x), list(x = run_age))),
        step = (if (is.null(run_step)) TRUE else f_substitute(quote(cur_step == x), list(x = run_step))),
        proj = (if (isFALSE(run_projection)) quote(!cur_year_projection) else TRUE),
        end = NULL))

    # NB: Generate action name with our arguments
    out <- list()
    action_name <- unique_action_name()
    out[[step_id(run_at, stock, action_name)]] <- g3a_renewal(
        stock,
        num_f = g3a_renewal_len_dnorm(mean_f, stddev_f, factor_f),
        wgt_f = g3a_renewal_wgt_wl(alpha_f, beta_f),
        run_f = run_f,
        run_at = run_at)[[1]]
    return(out)
}

# normalparam, but with a cv_f instead of stddev_f
g3a_renewal_normalcv <- function (
        stock,
        factor_f = g3_parameterized('rec',
            by_stock = by_stock,
            by_year = TRUE,
            scale = g3_parameterized(
                name = 'rec.scalar',
                by_stock = by_stock),
            ifmissing = NaN),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        cv_f = g3_parameterized('lencv', by_stock = by_stock, value = 0.1,
            optimise = FALSE),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        wgt_by_stock = TRUE,
        run_age = quote(stock__minage),
        run_projection = FALSE,
        run_step = 1,
        run_f = NULL,
        run_at = g3_action_order$renewal) {
    g3a_renewal_normalparam(
        stock,
        factor_f = factor_f,
        mean_f = mean_f,
        stddev_f = f_substitute(quote(mean_f * cv_f), list(mean_f = mean_f, cv_f = cv_f)),
        alpha_f = alpha_f,
        beta_f = beta_f,
        by_stock = by_stock,
        wgt_by_stock = wgt_by_stock,
        run_age = run_age,
        run_projection = run_projection,
        run_step = run_step,
        run_f = run_f,
        run_at = run_at)
}

#######################################  g3a_otherfood
# Assign number / mean weight based on formulae
g3a_otherfood <- function (
        stock,
        num_f = g3_parameterized('of_abund', by_year = TRUE, by_stock = by_stock,
            scale = g3_parameterized('of_abund.step', by_step = TRUE,
                by_stock = by_stock) ),
        wgt_f = g3_parameterized('of_meanwgt', by_stock = by_stock),
        by_stock = TRUE,
        run_f = quote( cur_time <= total_steps ),
        run_at = g3_action_order$initial) {
    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)

    if (!any(grepl("__midlen$", all.vars(num_f)))) {
        # num_f doesn't mention stock__midlen, we should add it
        num_f <- f_substitute(quote(n + 0 * stock__midlen), list(n = num_f))
    }
    out <- list()
    action_name <- unique_action_name()
    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_otherfood for ", stock)
        stock_iterate(stock, if (run_f && renew_into_f) {
            stock_ss(stock__num) <- num_f
            stock_ss(stock__wgt) <- wgt_f
        })
    }, list(
        num_f = num_f,
        wgt_f = wgt_f,
        renew_into_f = renewal_into(stock),
        run_f = run_f)))
    return(out)
}

# Steps to set up renewal of stocks on first step
g3a_otherfood_normalparam <- function (
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        stddev_f = g3_parameterized('init.sd', value = 10,
            by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        age_offset = quote( cur_step_size ),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = quote( cur_time <= total_steps ),
        run_at = g3_action_order$initial) {

    # Replace "age" with "age - cur_step_size", i.e. pretending this is happening at time "-1"
    if (!is.null(age_offset)) {
        age_offset <- f_substitute(quote(age - age_offset), list(age_offset = age_offset))
        mean_f <- f_substitute(mean_f, list(age = age_offset))
        stddev_f <- f_substitute(stddev_f, list(age = age_offset))
    }

    # NB: Generate action name with our arguments
    out <- list()
    action_name <- unique_action_name()
    out[[step_id(run_at, stock, action_name)]] <- g3a_otherfood(
        stock,
        num_f = g3a_renewal_len_dnorm(mean_f, stddev_f, factor_f),
        wgt_f = g3a_renewal_wgt_wl(alpha_f, beta_f),
        run_f = run_f,
        run_at = run_at)[[1]]
    return(out)
}

# normalparam, but with a cv_f instead of stddev_f
g3a_otherfood_normalcv <- function (
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        cv_f = g3_parameterized('lencv', by_stock = by_stock, value = 0.1,
            optimise = FALSE),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        age_offset = quote( cur_step_size ),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = quote( cur_time <= total_steps ),
        run_at = g3_action_order$initial) {
    g3a_otherfood_normalparam(
        stock = stock,
        factor_f = factor_f,
        mean_f = mean_f,
        stddev_f = f_substitute(quote(mean_f * cv_f), list(mean_f = mean_f, cv_f = cv_f)),
        alpha_f = alpha_f,
        beta_f = beta_f,
        age_offset = age_offset,
        run_f = run_f,
        run_at = run_at)
}
