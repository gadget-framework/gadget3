# Generate function saying whether this lengthgroup should be renewed into
renewal_into <- function (stock) {
    if (!is.null(stock$dim$tag)) return(~stock__tag_idx == stock__untagged_idx)
    return(TRUE)
}

# Parameterised vonb formula, for use as mean_f
g3a_renewal_vonb <- function(
        Linf = g3_parameterized('Linf', by_stock = by_stock),
        K = g3_parameterized('K', by_stock = by_stock, scale = 0.001),
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

g3a_renewal_initabund <- function(
    scalar = g3_parameterized('init.scalar', by_stock = by_stock),
    init = g3_parameterized('init', by_stock = by_stock, by_age = TRUE),
    M = g3_parameterized('M', by_stock = by_stock),
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
        stddev_f = g3_parameterized('init.sd', by_stock = by_stock, by_age = by_age),
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        by_stock = TRUE,
        by_age = FALSE) {
    dnorm <- f_substitute(
        quote( (stock__midlen - mean_f)/stddev_f ),
        list(mean_f = mean_f, stddev_f = stddev_f))
    g3_formula(
        quote( normalize_vec(exp( -(dnorm**2) * 0.5)) * 10000 * factor ),
        dnorm = dnorm,
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
g3a_initialconditions <- function (stock, num_f, wgt_f, run_f = ~cur_time == 0L, run_at = 0) {
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
        mean_f = g3a_renewal_vonb(by_stock = by_stock),
        stddev_f = g3_parameterized('init.sd', by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~cur_time == 0L,
        run_at = 0) {

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

# Assign number / mean weight based on formulae
g3a_renewal <- function (stock, num_f, wgt_f, run_f = ~TRUE, run_at = 8) {
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
        factor_f,
        mean_f,
        stddev_f = g3_parameterized('rec.sd', by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~TRUE,
        run_at = 8) {

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
