g3a_mature_constant <- function (alpha = NULL, l50 = NA, beta = NULL, a50 = NA, gamma = NULL, k50 = NA) {
    inner_code <- quote(0)

    if (!is.null(alpha)) {
        if (identical(l50, NA)) stop("l50 must be supplied if alpha is supplied")
        inner_code <- f_substitute(~inner_code - (alpha)*(stock__midlen - (l50)), list(
            alpha = alpha,
            l50 = l50,
            inner_code = inner_code))
    }

    if (!is.null(beta)) {
        if (identical(a50, NA)) stop("a50 must be supplied if beta is supplied")
        inner_code <- f_substitute(~inner_code - (beta)*(age - (a50)), list(
            beta = beta,
            a50 = a50,
            inner_code = inner_code))
    }

    if (!is.null(gamma)) {
        if (identical(k50, NA)) stop("k50 must be supplied if gamma is supplied")
        inner_code <- f_substitute(~inner_code - (gamma)*(stock_ss(stock__wgt) - (k50)), list(
            gamma = gamma,
            k50 = k50,
            inner_code = inner_code))
    }

    f_substitute(~1/(1 + exp(inner_code)), list(
        inner_code = inner_code))
}


# Finish a growth / age maturity step, add transitioned fish to new stock
g3a_step_transition <- function(input_stock,
                           output_stocks,
                           output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)),
                           run_f = TRUE) {
    return(f_concatenate(lapply(seq_along(output_stocks), function (n) {
        input_stock <- input_stock
        output_stock <- output_stocks[[n]]

        g3_step(f_substitute(~{
            debug_trace("Move ", input_stock ," to ", output_stock)
            stock_iterate(output_stock, stock_intersect(input_stock, if (run_f) {
                # Total biomass
                stock_ss(output_stock__wgt) <- (stock_ss(output_stock__wgt) * stock_ss(output_stock__num)) +
                    stock_reshape(output_stock, stock_ss(input_stock__transitioning_wgt) * stock_ss(input_stock__transitioning_num) * output_ratio)
                # Add numbers together
                stock_ss(output_stock__num) <- stock_ss(output_stock__num) + stock_reshape(output_stock, stock_ss(input_stock__transitioning_num) * output_ratio)
                # Back down to mean biomass
                stock_ss(output_stock__wgt) <- stock_ss(output_stock__wgt) / logspace_add_vec(stock_ss(output_stock__num), 0)
            }))
        }, list(run_f = run_f, output_ratio = output_ratios[[n]])))
    })))
}

# Growth step for a stock
# - stock: Input stock to mature
# - output_stocks: g3_stock / list of g3_stocks that mature individuals end up in
# - maturity_f: formula for proportion of length vector maturing
# - output_ratios: Proportions of matured fish that end up in each output stock. Either a list of formulae or values summing to 1. Default evenly spread across all.
g3a_mature <- function(stock, maturity_f, output_stocks, output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)), run_f =~TRUE, run_at = 5, transition_at = 7) {
    # Single stock --> list
    if (!is.null(output_stocks$name)) output_stocks <- list(output_stocks)

    # Check output_ratios matches output_stocks
    if (length(output_stocks) != length(output_ratios)) {
        stop("Number of output_stocks (", length(output_stocks), ") doesn't match output_ratios (", length(output_ratios), ")")
    }
    list_of_formulae <- function (x) all(vapply(x, rlang::is_formula, logical(1)))
    stopifnot(list_of_formulae(output_ratios) || sum(output_ratios) == 1)

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)
    stock__transitioning_num <- stock_instance(stock)
    stock__transitioning_wgt <- stock_instance(stock)

    out <- new.env(parent = emptyenv())
    out[[step_id(run_at, 1, stock)]] <- g3_step(f_substitute(~{
        debug_label("g3a_mature for ", stock)
        debug_trace("Reset transitioning storage")
        stock_with(stock, stock__transitioning_num[] <- 0)
        stock_with(stock, stock__transitioning_wgt[] <- stock__wgt[])

        stock_iterate(stock, if (run_f) {
            debug_trace("Move matured ", stock, " into temporary storage")
            stock_ss(stock__num) <- stock_ss(stock__num) - (stock_ss(stock__transitioning_num) <- stock_ss(stock__num) * (maturity_f))
        })
    }, list(run_f = run_f, maturity_f = maturity_f)))

    # Shunt matured into their own stock
    out[[step_id(transition_at, 2, stock)]] <- g3a_step_transition(stock, output_stocks, output_ratios, run_f = run_f)
    return(as.list(out))
}
