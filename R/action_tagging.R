g3a_predate_tagrelease <- function (
        fleet_stock,
        prey_stocks,
        suitabilities,
        catchability_f,
        output_tag_f,
        mortality_f = 0,
        run_f = ~TRUE,
        run_at = g3_action_order$predate,
        ...) {
    stopifnot(g3_is_stock(fleet_stock))
    stopifnot(is.list(prey_stocks) && all(sapply(prey_stocks, g3_is_stock)))
    stopifnot(is.numeric(output_tag_f) || rlang::is_formula(output_tag_f))
    if (is.numeric(output_tag_f)) output_tag_f <- as.integer(output_tag_f)
    predstock <- fleet_stock   # Preserve historical function signature

    # Start with a regular predation function
    out <- g3a_predate_fleet(
        fleet_stock = predstock,
        prey_stocks = prey_stocks,
        suitabilities = suitabilities,
        catchability_f = catchability_f,
        run_f = run_f,
        run_at = run_at,
        ...)

    for (stock in prey_stocks) {
        # Move captured stock back into tagged dimension
        out[[step_id(run_at, 11, stock, predstock)]] <- g3_step(f_substitute(~{
            debug_label("Release ", stock, " caught by ", predstock, " with tags")

            stock_iterate(stock, if (run_f) g3_with(
                    # Numbers caught (stock__predby_predstock is total weight)
                    output_tag_idx := tag_idx_f,
                    tagged_num := stock_ss(stock__predby_predstock) / avoid_zero_vec(stock_ss(stock__wgt)), {
                stock_ss(stock__wgt, tag = output_tag_idx) <- ratio_add_vec(
                    stock_ss(stock__wgt, tag = output_tag_idx), stock_ss(stock__num, tag = output_tag_idx),
                    stock_ss(stock__wgt), (1.0 - mortality_f) * tagged_num)
                stock_ss(stock__num, tag = output_tag_idx) <-
                    stock_ss(stock__num, tag = output_tag_idx) + (1.0 - mortality_f) * tagged_num
            }))
        }, list(
            stock__predby_predstock = as.symbol(paste0('stock__predby_', predstock$name)),
            mortality_f = if (identical(mortality_f, 0)) 0 else g3a_naturalmortality_exp(mortality_f, action_step_size_f = 1),
            tag_idx_f = g3s_tag_reverse_lookup(stock, output_tag_f),
            run_f = run_f)))
    }
    return(as.list(out))
}

g3a_tag_shedding <- function (
        stocks,
        tagshed_f,
        run_f = ~TRUE,
        run_at = g3_action_order$straying) {  # TODO: it's kinda like straying, but it's own home would be more sensible
    stopifnot(is.list(stocks) && all(sapply(stocks, g3_is_stock)))

    out <- new.env(parent = emptyenv())

    for (stock in stocks) {
        out[[step_id(run_at, 'g3a_tag_shedding', stock)]] <- g3_step(f_substitute(~{
            debug_label("Shed tags from ", stock)
            stock_iterate(stock, if (stock__tag_idx != stock__untagged_idx && run_f) {
                # Work out how many tags have been shedded from this length group
                g3_with(tags_shedded := stock_ss(stock__num) * exp(-tagshed_f), {
                    # Move from current tag back to untagged
                    stock_ss(stock__num) <- stock_ss(stock__num) - tags_shedded
                    stock_ss(stock__num, tag = stock__untagged_idx) <-
                        stock_ss(stock__num, tag = stock__untagged_idx) + tags_shedded
                })
            })
        }, list(
            tagshed_f = tagshed_f,
            run_f = run_f)))
    }
    return(as.list(out))
}
