g3a_mature_continuous <- function (
        alpha = g3_parameterized('mat.alpha', by_stock = by_stock),
        l50 = g3_parameterized('mat.l50', by_stock = by_stock),
        beta = 0,
        a50 = 0,
        by_stock = TRUE) {
    # https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#continuous-maturity-function
    # https://github.com/Hafro/gadget2/blob/master/src/maturity.cc#L301-L304

    g3a_mature_continuous <- g3_native(r = function (plusdl, m0, growth_l, alpha, beta, cur_step_size) {
        # ldelta: Absolute increase in length for each entry in growth_l
        # (i.e. growth * LgrpDiv->dl() in MaturityA::calcMaturation
        # TODO: Not considering __dl when we have it
        ldelta <- plusdl * seq(0, ncol(growth_l) - 1)  # 0..maxlengthgrouplen increases

        # NB: m0 == preCalcMaturation(maturity.cc:275)
        return(m0 %*% t(alpha * ldelta + beta * cur_step_size))
    }, cpp = '[](Type plusdl, vector<Type> m0, array<Type> growth_l, Type alpha, Type beta, Type cur_step_size) -> array<Type> {
        vector<Type> ldelta(growth_l.cols());
        array<Type> out(m0.size(), growth_l.cols());

        for (int i = 0 ; i < ldelta.size(); i++) {
            ldelta[i] = plusdl * (double)i;
        }

        // NB: Need to declare/assign/return separately to get Eigen types right
        out = (m0.matrix() * (alpha * ldelta + beta * cur_step_size).matrix().transpose()).array();
        return(out);
    }')

    f_substitute(~g3a_mature_continuous(stock__plusdl, M0, growth_delta_l, alpha, beta, cur_step_size), list(
        M0 = g3a_mature_constant(alpha, l50, beta, a50),
        alpha = alpha,
        beta = beta))
}

g3a_mature_constant <- function (alpha = NULL, l50 = NA, beta = NULL, a50 = NA, gamma = NULL, k50 = NA) {
    # https://github.com/Hafro/gadget2/blob/master/src/maturity.cc#L530
    inner_code <- quote(0)

    if (!is.null(alpha) && alpha != 0) {
        if (identical(l50, NA)) stop("l50 must be supplied if alpha is supplied")
        inner_code <- f_substitute(~inner_code - (alpha)*(stock__midlen - (l50)), list(
            alpha = alpha,
            l50 = l50,
            inner_code = inner_code))
    }

    if (!is.null(beta) && beta != 0) {
        if (identical(a50, NA)) stop("a50 must be supplied if beta is supplied")
        inner_code <- f_substitute(~inner_code - (beta)*(age - (a50)), list(
            beta = beta,
            a50 = a50,
            inner_code = inner_code))
    }

    if (!is.null(gamma) && gamma != 0) {
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
                           move_remainder = TRUE,
                           run_f = TRUE) {
    input_stock__transitioning_remainder <- g3_stock_instance(input_stock)

    if (length(output_stocks) == 1) {
        # Simplified case for single output stock, no need to manage remainder
        n <- 1
        output_stock <- output_stocks[[n]]
        output_stock__num <- g3_stock_instance(output_stock, 0)
        output_stock__wgt <- g3_stock_instance(output_stock, 1)

        return(g3_step(f_substitute(~{
            debug_trace("Move ", input_stock ," to ", output_stock)
            stock_iterate(output_stock, stock_intersect(input_stock, if (run_f) {
                # Total biomass
                stock_ss(output_stock__wgt) <- (stock_ss(output_stock__wgt) * stock_ss(output_stock__num)) +
                    stock_reshape(output_stock, stock_ss(input_stock__transitioning_wgt) * stock_ss(input_stock__transitioning_num) * output_ratio)
                # Add numbers together
                stock_ss(output_stock__num) <- stock_ss(output_stock__num) + stock_reshape(output_stock, stock_ss(input_stock__transitioning_num) * output_ratio)
                # NB: Assuming that there won't be leftover length
                if (move_remainder) stock_ss(input_stock__transitioning_num) <- stock_ss(input_stock__transitioning_num) - stock_ss(input_stock__transitioning_num) * output_ratio
                # Back down to mean biomass
                stock_ss(output_stock__wgt) <- stock_ss(output_stock__wgt) / avoid_zero_vec(stock_ss(output_stock__num))
            }))

            if (move_remainder) {
                debug_trace("Move any unclaimed stock back to ", input_stock)
                stock_with(input_stock, if (run_f) {
                    input_stock__num <- input_stock__num + input_stock__transitioning_num
                })
            }
        }, list(
            output_ratio = output_ratios[[n]],
            move_remainder = move_remainder,
            run_f = run_f))))
    }

    out <- c(
        list(g3_step(f_substitute(~{
            if (move_remainder) stock_with(input_stock, input_stock__transitioning_remainder <- input_stock__transitioning_num)
        }, list(
            move_remainder = move_remainder,
            run_f = run_f)), recursing = TRUE)),
        lapply(seq_along(output_stocks), function (n) {
            # NB: Pull these from parent env so g3_step can find them
            input_stock <- input_stock
            output_stock <- output_stocks[[n]]
            output_stock__num <- g3_stock_instance(output_stock, 0)
            output_stock__wgt <- g3_stock_instance(output_stock, 1)

            g3_step(f_substitute(~{
                debug_trace("Move ", input_stock ," to ", output_stock)
                stock_iterate(output_stock, stock_intersect(input_stock, if (run_f) {
                    # Total biomass
                    stock_ss(output_stock__wgt) <- (stock_ss(output_stock__wgt) * stock_ss(output_stock__num)) +
                        stock_reshape(output_stock, stock_ss(input_stock__transitioning_wgt) * stock_ss(input_stock__transitioning_num) * output_ratio)
                    # Add numbers together
                    stock_ss(output_stock__num) <- stock_ss(output_stock__num) + stock_reshape(output_stock, stock_ss(input_stock__transitioning_num) * output_ratio)
                    # NB: Assuming that there won't be leftover length
                    if (move_remainder) stock_ss(input_stock__transitioning_remainder) <- stock_ss(input_stock__transitioning_remainder) - stock_ss(input_stock__transitioning_num) * output_ratio
                    # Back down to mean biomass
                    stock_ss(output_stock__wgt) <- stock_ss(output_stock__wgt) / avoid_zero_vec(stock_ss(output_stock__num))
                }))
            }, list(
                output_ratio = output_ratios[[n]],
                move_remainder = move_remainder,
                run_f = run_f)), recursing = TRUE)
        }),
        list(g3_step(f_substitute(~{
            if (move_remainder) {
                debug_trace("Move any unclaimed stock back to ", input_stock)
                stock_with(input_stock, if (run_f) {
                    input_stock__num <- input_stock__num + input_stock__transitioning_remainder
                })
            }
        }, list(
            move_remainder = move_remainder,
            run_f = run_f)), recursing = TRUE)))
    # Assign names to each part, and use f_substitute to combine them
    if (length(out) > length(c(letters, LETTERS))) stop("Not enough temporary names to assign to each part")
    names(out) <- c(letters, LETTERS)[1:length(out)]
    g3_step(f_substitute(
        as.call(c(as.symbol("{"), lapply(names(out), as.symbol))),  # }
        out))
}

# Growth step for a stock
# - stock: Input stock to mature
# - output_stocks: g3_stock / list of g3_stocks that mature individuals end up in
# - maturity_f: formula for proportion of length vector maturing
# - output_ratios: Proportions of matured fish that end up in each output stock. Either a list of formulae or values summing to 1. Default evenly spread across all.
g3a_mature <- function(stock, maturity_f, output_stocks, output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)), run_f =~TRUE, run_at = g3_action_order$grow, transition_at = g3_action_order$mature) {
    # Single stock --> list
    if (!is.null(output_stocks$name)) output_stocks <- list(output_stocks)

    # Check output_ratios matches output_stocks
    if (length(output_stocks) != length(output_ratios)) {
        stop("Number of output_stocks (", length(output_stocks), ") doesn't match output_ratios (", length(output_ratios), ")")
    }
    list_of_formulae <- function (x) all(vapply(x, rlang::is_formula, logical(1)))
    stopifnot(list_of_formulae(output_ratios) || sum(output_ratios) == 1)

    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)
    stock__transitioning_num <- g3_stock_instance(stock)
    stock__transitioning_wgt <- g3_stock_instance(stock)

    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()
    out[[step_id(run_at, 1, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_mature for ", stock)
        debug_trace("Reset transitioning storage")
        stock_with(stock, stock__transitioning_num[] <- 0)
        stock_with(stock, stock__transitioning_wgt[] <- stock__wgt[])

        stock_iterate(stock, if (run_f) g3_with(maturity_ratio := maturity_f, {
            debug_trace("Move matured ", stock, " into temporary storage")
            stock_ss(stock__num) <- stock_ss(stock__num) - (stock_ss(stock__transitioning_num) <- stock_ss(stock__num) * maturity_ratio)
        }))
    }, list(run_f = run_f, maturity_f = maturity_f)))

    # Shunt matured into their own stock
    out[[step_id(transition_at, 2, stock)]] <- g3a_step_transition(stock, output_stocks, output_ratios, run_f = run_f)
    return(as.list(out))
}
