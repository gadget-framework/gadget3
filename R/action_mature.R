g3a_mature_constant <- function (alpha = 0, l50 = NA, beta = 0, a50 = NA, gamma = 0, k50 = NA) {
    inner_code <- quote(0)

    if (alpha > 0) {
        if (is.na(l50)) stop("l50 must be supplied if alpha > 0")
        inner_code <- substitute(inner_code - alpha*(stock__midlen - l50), list(
            alpha = alpha,
            l50 = l50,
            inner_code = inner_code))
    }

    if (beta > 0) {
        if (is.na(a50)) stop("a50 must be supplied if beta > 0")
        inner_code <- substitute(inner_code - beta*(age - a50), list(
            beta = beta,
            a50 = a50,
            inner_code = inner_code))
    }

    if (gamma > 0) {
        if (is.na(k50)) stop("k50 must be supplied if gamma > 0")
        inner_code <- substitute(inner_code - gamma*(stock__wgt[stock__iter] - k50), list(
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

        stock_step(f_substitute(~{
            stock_comment("Move ", input_stock ," to ", output_stock)
            stock_iterate(output_stock, stock_intersect(input_stock, if (run_f) {
                # Total biomass
                output_stock__wgt[output_stock__iter] <- (output_stock__wgt[output_stock__iter] * output_stock__num[output_stock__iter]) +
                    (input_stock__transitioning_wgt[input_stock__iter] * input_stock__transitioning_num[input_stock__iter] * output_ratio)
                # Add numbers together
                output_stock__num[output_stock__iter] <- output_stock__num[output_stock__iter] + (input_stock__transitioning_num[input_stock__iter] * output_ratio)
                # Back down to mean biomass
                output_stock__wgt[output_stock__iter] <- output_stock__wgt[output_stock__iter] / pmax(output_stock__num[output_stock__iter], 0.00001)
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

    stock__transitioning_num <- stock_definition(stock, 'stock__num')
    stock__transitioning_wgt <- stock_definition(stock, 'stock__wgt')

    out <- new.env(parent = emptyenv())
    out[[step_id(run_at, 1, stock)]] <- stock_step(f_substitute(~{
        stock_comment("g3a_mature for ", stock)
        # Reset transitioning stock
        stock_with(stock, stock__transitioning_num[] <- 0)
        stock_with(stock, stock__transitioning_wgt[] <- stock__wgt[])

        stock_iterate(stock, if (run_f) {
            stock_comment("Move matured ", stock, " into temporary storage")
            stock__num[stock__iter] <- stock__num[stock__iter] - (stock__transitioning_num[stock__iter] <- stock__num[stock__iter] * maturity_f)
        })
    }, list(run_f = run_f, maturity_f = maturity_f)))

    # Shunt matured into their own stock
    out[[step_id(transition_at, 2, stock)]] <- g3a_step_transition(stock, output_stocks, output_ratios, run_f = run_f)
    return(as.list(out))
}
