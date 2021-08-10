g3a_spawn_recruitment_fecundity <- function (p0, p1, p2, p3, p4) {
    subs <- list(p0 = p0, p1 = p1, p2 = p2, p3 = p3, p4 = p4)

    # NB: ricker is calculated over an entire area, so divide up so each age/length spawn equally.
    list(
        s = f_substitute(~sum(
                stock__midlen ^ p1 *
                age ^ p2 *
                stock_ss(stock__spawningnum) ^ p3 *
                stock_ss(stock__wgt) ^ p4), subs),
        r = f_substitute(~p0 * s, subs))
}

g3a_spawn_recruitment_simplessb <- function (mu) {
    # NB: simplessb is calculated over an entire area, so divide up so each age/length spawn equally.
    list(
        s = ~sum(stock_ss(stock__wgt) * stock_ss(stock__spawningnum)),
        r = f_substitute(~mu * s, list(
            mu = mu)))
}

# https://github.com/gadget-framework/gadget-course/blob/master/stock_interactions.Rmd#L81
# SpawnData::calcRecruitNumber() line 466
g3a_spawn_recruitment_ricker <- function (mu, lambda) {
    # NB: ricker is calculated over an entire area, so divide up so each age/length spawn equally.
    list(
        s = ~sum(stock_ss(stock__wgt) * stock_ss(stock__spawningnum)),
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
    stopifnot(is.list(output_stocks) && all(sapply(output_stocks, g3_is_stock)))
    stopifnot(identical(names(recruitment_f), c('s', 'r')))
    stopifnot(length(output_stocks) == length(output_ratios))
    stopifnot(abs(sum(output_ratios) - 1) < 0.0001)

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)
    stock__spawningnum <- stock_instance(stock, desc = "Number of spawning parents")
    stock__offspringnum <- stock_instance(stock, desc = "Total number of offspring these parents produce")
    out <- list()
    action_name <- unique_action_name()

    # See SpawnData::Spawn line 286
    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("Collect total spawned offspring from ", stock, " spawning event")

        debug_trace("Calculate spawning proportion of ", stock)
        stock_iterate(stock, if (run_f) {
            stock_ss(stock__spawningnum) <- stock_ss(stock__num) * proportion_f
        } else {
            stock_ss(stock__spawningnum) <- 0
        })

        debug_trace("Calculate total offspring of spawning population")
        # Equivalent to spawner::calcRecriutNumber()
        g3_with(s := 0 * nll, {  # TODO: Ugly mess to get type right
            stock_iterate(stock, if (run_f) {
                s <- s + recruitment_s_f
                stock_ss(stock__offspringnum) <- 1
            } else {
                stock_ss(stock__offspringnum) <- 0
            })
            g3_with(r := recruitment_r_f,
                stock_with(stock, stock__offspringnum <- r * stock__offspringnum / avoid_zero(sum(stock__offspringnum))))
        })

        stock_iterate(stock, if (run_f) {
            if (weightloss_enabled) {
                debug_trace("Apply weight loss to parents")
                # Spawndata::Spawn, pop
                stock_ss(stock__wgt) <- ratio_add_vec(
                    stock_ss(stock__wgt),
                    stock_ss(stock__num) - stock_ss(stock__spawningnum),
                    stock_ss(stock__wgt) - stock_ss(stock__wgt) * weightloss_f,
                    stock_ss(stock__spawningnum))
            }
            if (mortality_enabled) {
                debug_trace("Apply spawning mortality to parents")
                # Spawndata::Spawn, pop
                stock_ss(stock__num) <-
                    (stock_ss(stock__num) - stock_ss(stock__spawningnum)) +
                    stock_ss(stock__spawningnum) * exp(-mortality_f)
            }
        })
    }, list(
        recruitment_s_f = recruitment_f$s,
        recruitment_r_f = recruitment_f$r,
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
        output_stock__spawnednum <- stock_instance(output_stock, desc = "Individuals spawned")

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
            stock_with(output_stock, stock_with(stock,
                output_stock__spawnednum <- (output_stock__spawnednum / avoid_zero(sum_all_outputs_f)) * sum(stock__offspringnum) * output_ratio))
            stock_iterate(output_stock, if (run_f && output_stock_cond) {
                stock_ss(output_stock__wgt) <- ratio_add_vec(
                    stock_ss(output_stock__wgt),
                    stock_ss(output_stock__num),
                    (alpha_f) * output_stock__midlen ** (beta_f),
                    stock_ss(output_stock__spawnednum))
                stock_ss(output_stock__num) <- stock_ss(output_stock__num) + stock_ss(output_stock__spawnednum)
            })
        }, list(
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
