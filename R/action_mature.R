
g3a_mature_continuous <- function (alpha, l50, beta, a50) {

}

# Growth step for a stock
# - growth_f: formulae for growth, e.g. g3a_grow_lengthvbsimple()
# - impl_f: formulae for growth implmentation, e.g. g3a_grow_impl_bbinom()
g3a_mature <- function(input_stock, output_stocks, maturity_f, maturity_steps = NULL) {
    # Single stock case, turn back into data.frame
    if (!is.data.frame(output_stocks)) output_stocks <- data.frame(stocks = I(list(output_stocks)), ratios = 1)

    # Make a temporary stock to store the outputs
    matured <- stock_clone(input_stock, name = paste0('matured_', input_stock$name))

    out <- new.env(parent = emptyenv())
    out$step056 <- stock_step(input_stock, extra_stock = matured, iter_f = f_substitute(~{
        comment(stock_comment)
        input_stock__num[input_stock__iter] <- input_stock__num[input_stock__iter] - (matured__num[matured__iter] <- input_stock__num[input_stock__iter] * maturity_f)
        input_stock__wgt[input_stock__iter] <- input_stock__wgt[input_stock__iter] - (matured__wgt[matured__iter] <- input_stock__wgt[input_stock__iter] * maturity_f)
    }, list(
        stock_comment = paste0("Move matured ", input_stock$name ," into temporary storage"),
        maturity_f = maturity_f)))

    for (n in seq_len(nrow(output_stocks))) {
        output_stock <- output_stocks$stocks[[n]]
        output_ratio <- output_stocks$ratios[[n]]

        assign(paste0('step075:', output_stock$name), stock_step(output_stock, extra_stock = matured, iter = f_substitute(~{
            comment(stock_comment)
            output_stock__num[output_stock__iter] <- output_stock__num[output_stock__iter] + matured__num[matured__iter] * output_ratio
            output_stock__wgt[output_stock__iter] <- output_stock__wgt[output_stock__iter] + matured__wgt[matured__iter] * output_ratio
        }, list(
            stock_comment = paste0("Move matured ", input_stock$name ," to ", output_stock$name),
            output_ratio = output_ratio))), envir = out)
    }

    return(as.list(out))
}
