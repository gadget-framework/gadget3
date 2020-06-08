g3a_mature_constant <- function (alpha = 0, l50 = NA, beta = 0, a50 = NA, gamma = 0, k50 = NA) {
    inner_code <- quote(0)

    if (alpha > 0) {
        if (is.na(l50)) stop("l50 must be supplied if alpha > 0")
        inner_code <- substitute(inner_code - alpha*(stock__meanlen - l50), list(
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
        if (is.na(k50)) stop("l50 must be supplied if gamma > 0")
        inner_code <- substitute(inner_code - gamma*(stock__wgt[stock__iter] - k50), list(
            beta = beta,
            a50 = a50,
            inner_code = inner_code))
    }

    f_substitute(~1/(1 + exp(inner_code)), list(
        inner_code = inner_code))
}

# Growth step for a stock
# - growth_f: formulae for growth, e.g. g3a_grow_lengthvbsimple()
# - impl_f: formulae for growth implmentation, e.g. g3a_grow_impl_bbinom()
g3a_mature <- function(stock, output_stocks, maturity_f, maturity_steps = NULL) {
    # Single stock case, turn back into data.frame
    if (!is.data.frame(output_stocks)) output_stocks <- data.frame(stocks = I(list(output_stocks)), ratios = 1)

    # Make a temporary stock to store the outputs
    matured <- stock_clone(stock, name = paste0('matured_', stock$name))

    out <- new.env(parent = emptyenv())
    out[[paste0('070:1:', stock$stock_name)]] <- stock_step(stock, extra_stock = matured, iter_f = f_substitute(~{
        comment(stock_comment)
        stock__num[stock__iter] <- stock__num[stock__iter] - (matured__num[matured__iter] <- stock__num[stock__iter] * maturity_f)
        stock__wgt[stock__iter] <- stock__wgt[stock__iter] - (matured__wgt[matured__iter] <- stock__wgt[stock__iter] * maturity_f)
    }, list(
        stock_comment = paste0("Move matured ", stock$name ," into temporary storage"),
        maturity_f = maturity_f)))

    for (n in seq_len(nrow(output_stocks))) {
        output_stock <- output_stocks$stocks[[n]]
        output_ratio <- output_stocks$ratios[[n]]

        assign(paste0('070:2:', output_stock$name), stock_step(output_stock, extra_stock = matured, iter = f_substitute(~{
            comment(stock_comment)
            output_stock__num[output_stock__iter] <- output_stock__num[output_stock__iter] + matured__num[matured__iter] * output_ratio
            output_stock__wgt[output_stock__iter] <- output_stock__wgt[output_stock__iter] + matured__wgt[matured__iter] * output_ratio
        }, list(
            stock_comment = paste0("Move matured ", stock$name ," to ", output_stock$name),
            output_ratio = output_ratio))), envir = out)
    }

    return(as.list(out))
}
