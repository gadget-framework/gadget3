g3a_report_stock <- function (report_stock, input_stock, instance_name, run_f = TRUE, run_at = 11) {
    out <- new.env(parent = emptyenv())

    input_stock_instance_name <- paste0('input_stock__', instance_name)
    report_stock_instance_name <- paste0('report_stock__', instance_name)
    assign(report_stock_instance_name, stock_instance(report_stock))

    out[[step_id(run_at, report_stock, instance_name)]] <- g3_step(f_substitute(~{
        if (run_f) {
            stock_comment("Fill in report ", report_stock, " from ", input_stock)
            if (cur_time == 0L) {
                stock_with(report_stock, report_instance[] <- 0)
            }
            stock_iterate(input_stock, stock_intersect(report_stock, {
                stock_ss(report_instance) <- stock_ss(report_instance) + stock_ss(input_instance)
            }))
            stock_with(report_stock, g3_report(report_instance))
        }
    }, list(
        input_instance = as.symbol(input_stock_instance_name),
        report_instance = as.symbol(report_stock_instance_name),
        run_f = run_f)))

    return(as.list(out))
}
