# https://github.com/gadget-framework/gadget-course/blob/master/stock_interactions.Rmd#L81
# SpawnData::calcRecruitNumber() line 466
g3a_spawn_recriutment_ricker <- function (mu, lambda) {
    # NB: ricker is calculated over an entire area, so divide up so each age/length spawn equally.
    list(
        s = ~sum(stock_ss(stock__wgt) * stock_ss(stock__num) * proportion_f),
        r = f_substitute(~mu * s * exp(-lambda * s), list(
            mu = mu,
            lambda = lambda)))
}

g3a_spawn_length_straightline <- function (alpha, beta) {
    f_substitute(~alpha * stock__midlen + beta, list(
        alpha = alpha,
        beta = beta))
}

g3a_spawn_length_exponential <- function (alpha, l50) {
    f_substitute(~1 / ( 1 + exp(alpha * (stock__midlen - l50)) ), list(
        alpha = alpha,
        l50 = l50))
}

g3a_spawn <- function(
        stock,
        recruitment_f,
        proportion_f = 1,
        mortality_f = 0,
        weightloss_f = 0,
        output_stocks = list(),
        output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)),
        mean_f, stddev_f, alpha_f, beta_f,
        run_f = ~TRUE,
        run_at = 6,
        recruit_at = 8) {
    stopifnot("g3_stock" %in% class(stock))
    stopifnot(is.list(output_stocks) && all(sapply(output_stocks, function (x) "g3_stock" %in% class(x))))
    stopifnot(identical(names(recruitment_f), c('s', 'r')))
    stopifnot(length(output_stocks) == length(output_ratios))
    stopifnot(abs(sum(output_ratios) - 1) < 0.0001)

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)
    out <- list()
    action_name <- unique_action_name()
    totalspawn_var_name <- paste(stock$name, action_name, "totalspawn", sep = "_")
    assign(totalspawn_var_name, structure(0, desc = "Total offspring for this spawning event"))

    # See SpawnData::Spawn line 286
    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("Collect total spawned offspring from ", stock, " spawning event")
        # Equivalent to spawner::calcRecriutNumber()
        stock_with(stock, totalspawn_var <- 0)
        stock_iterate(stock, if (run_f) {
            totalspawn_var <- totalspawn_var + recriutment_s_f
        })
        stock_with(stock, totalspawn_var <- recriutment_r_f)

        stock_iterate(stock, if (run_f) {
            if (weightloss_enabled) {
                debug_trace("Apply weight loss to parents")
                # Spawndata::Spawn, pop
                stock_ss(stock__wgt) <- ratio_add_vec(
                    stock_ss(stock__wgt),
                    1 - proportion_f,
                    stock_ss(stock__wgt) - stock_ss(stock__wgt) * weightloss_f,
                    proportion_f)
            }
            if (mortality_enabled) {
                debug_trace("Apply spawning mortality to parents")
                # Spawndata::Spawn, pop
                stock_ss(stock__num) <-
                    stock_ss(stock__num) * (1 - proportion_f) +
                    stock_ss(stock__num) * proportion_f * exp(-mortality_f)
            }
        })
    }, list(
        totalspawn_var = as.symbol(totalspawn_var_name),
        recriutment_s_f = f_substitute(recruitment_f$s, list(proportion_f = proportion_f)),
        recriutment_r_f = f_substitute(recruitment_f$r, list(s = as.symbol(totalspawn_var_name))),
        proportion_f = proportion_f,
        mortality_enabled = !identical(mortality_f, 0),
        mortality_f = mortality_f,
        weightloss_enabled = !identical(weightloss_f, 0),
        weightloss_f = weightloss_f,
        run_f = run_f)))

    # Move spawned fish into their own stock
    out_f <- ~{}
    sum_all_outputs_f <- ~0
    for (i in seq_along(output_stocks)) {
        output_stock <- output_stocks[[i]]
        output_ratio <- output_ratios[[i]]
        output_stock__spawnednum <- stock_instance(output_stock)

        # Make a formula to sum all our outputs
        sum_all_outputs_f <- g3_step(f_substitute(~stock_with(output_stock, sum(output_stock__spawnednum)) + sum_all_outputs_f, list(
            sum_all_outputs_f = sum_all_outputs_f)))

        out_f <- g3_step(f_substitute(~{
            debug_trace("Generate normal distribution for spawned ", output_stock)
            # Equivalent to Spawner::Storage, pre-calcRecruitNumber()
            stock_with(output_stock, output_stock__spawnednum[] <- 0)
            stock_iterate(output_stock, if (run_f && output_stock_cond) {
                stock_ss(output_stock__spawnednum) <- exp(-(((output_stock__midlen - (mean_f)) * (1 / (stddev_f))) ** 2) * 0.5)
            })
            extension_point
            debug_trace("Scale total spawned stock by total to spawn in cycle")
            # sum_all_outputs_f equivalent to sum in Spawner::addSpawnStock()
            stock_with(output_stock,
                output_stock__spawnednum <- (output_stock__spawnednum / avoid_zero(sum_all_outputs_f)) * totalspawn_var * output_ratio)
            stock_iterate(output_stock, if (run_f && output_stock_cond) {
                stock_ss(output_stock__wgt) <- ratio_add_vec(
                    stock_ss(output_stock__wgt),
                    stock_ss(output_stock__num),
                    (alpha_f) * output_stock__midlen ** (beta_f),
                    stock_ss(output_stock__spawnednum))
                stock_ss(output_stock__num) <- stock_ss(output_stock__num) + stock_ss(output_stock__spawnednum)
            })
        }, list(
            totalspawn_var = as.symbol(totalspawn_var_name),
            mean_f = mean_f,
            stddev_f = stddev_f,
            alpha_f = alpha_f,
            beta_f = beta_f,
            output_ratio = output_ratio,
            output_stock_cond = ~age == output_stock__minage,
            run_f = run_f,
            extension_point = out_f)))
    }
    if (length(output_stocks) == 1) {
        output_stock <- output_stocks[[1]]
        # Label can mention single stock
        out_f <- g3_step(f_substitute(~{
            debug_label("Assign offspring from ", stock, " spawning event to ", output_stock)
            out_f
        }, list(out_f = out_f)))
    } else {
        out_f <- g3_step(f_substitute(~{
            debug_label("Assign offspring from ", stock, " spawning event to output stocks")
            out_f
        }, list(out_f = out_f)))
    }
    out[[step_id(recruit_at, stock, action_name)]] <- f_substitute(out_f, list(
        sum_all_outputs_f = f_optimize(sum_all_outputs_f)))

    return(out)
}
