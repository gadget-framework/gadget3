g3a_report_stock <- function (report_stock, input_stock, report_f, run_f = TRUE, run_at = 11) {
    out <- new.env(parent = emptyenv())

    # Find first stock__inst variable, and use that to name ours
    inst_var_name <- all.vars(report_f)
    inst_var_name <- inst_var_name[grepl('__', inst_var_name, fixed = TRUE)][[1]]
    if (!is.null(inst_var_name)) {
        instance_name <- gsub('^.*__', '', inst_var_name)
    } else {
        instance_name <- 'rep'
    }

    report_stock_instance_name <- paste0('report_stock__', instance_name)
    assign(report_stock_instance_name, stock_instance(report_stock))

    out[[step_id(run_at, report_stock, instance_name)]] <- g3_step(f_substitute(~{
        if (run_f) {
            stock_comment("Fill in report ", report_stock, " from ", input_stock)
            if (cur_time == 0L) {
                stock_with(report_stock, report_instance[] <- 0)
            }
            stock_iterate(input_stock, stock_intersect(report_stock, {
                stock_ss(report_instance) <- stock_ss(report_instance) + (report_f)
            }))
            stock_with(report_stock, g3_report(report_instance))
        }
    }, list(
        report_instance = as.symbol(report_stock_instance_name),
        report_f = report_f,
        run_f = run_f)))

    return(as.list(out))
}
