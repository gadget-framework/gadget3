g3a_predate_catchability_totalfleet <- function (E) {
    list(
        suit_unit = "total biomass",
        suit = quote( suit_f * stock_ss(stock__num) * stock_ss(stock__wgt) ),
        cons = f_substitute(
            ~stock_ss(predprey__suit) * (E / total_predsuit),
            list(E = E)) )
}

g3a_predate_catchability_numberfleet <- function (E) {
    list(
        suit_unit = "number of individuals",
        suit = quote( suit_f * stock_ss(stock__num) ),
        cons = f_substitute(
            ~stock_ss(predprey__suit) * (E / total_predsuit) * stock_ss(stock__wgt),
            list(E = E)) )
}

g3a_predate_catchability_linearfleet <- function (E) {
    list(
        suit_unit = "total biomass",
        suit = quote( suit_f * stock_ss(stock__num) * stock_ss(stock__wgt) ),
        cons = f_substitute(
            ~E * cur_step_size * stock_ss(predprey__suit),
            list(E = E)) )
}

g3a_predate_catchability_effortfleet <- function (catchability_fs, E) {
    list(
        suit_unit = "total biomass",
        suit = quote( suit_f * stock_ss(stock__num) * stock_ss(stock__wgt) ),
        cons = f_substitute(
            ~catchability_fs * E * cur_step_size * stock_ss(predprey__suit),
            list(
                catchability_fs = list_to_stock_switch(catchability_fs),
                E = E)) )
}

g3a_predate_catchability_quotafleet <- function (quota_table, E, sum_stocks = list(), recalc_f = NULL) {
    stopifnot(is.data.frame(quota_table) && identical(names(quota_table), c('biomass', 'quota')))
    stopifnot(nrow(quota_table) > 0 && is.infinite(quota_table[nrow(quota_table), 'biomass']))

    stopifnot(is.list(sum_stocks) && all(sapply(sum_stocks, g3_is_stock)))
    stopifnot(is.null(recalc_f) || rlang::is_formula(recalc_f))

    # Generate code to produce biomass sum
    if (length(sum_stocks) == 0) {
        biomass_c <- quote(sum(stock__num * stock__wgt))
    } else {
        biomass_c <- 0
        for (stock in sum_stocks) {
            biomass_c <- substitute(stock_with(s, sum(s__num * s__wgt)) + biomass_c, list(
                s = as.symbol(stock$name),
                s__num = as.symbol(paste0(stock$name, "__num")),
                s__wgt = as.symbol(paste0(stock$name, "__wgt")),
                biomass_c = biomass_c))
        }
    }

    # Make tertiary condition encoding quota
    for (i in rev(seq_len(nrow(quota_table)))) {
        if (i == nrow(quota_table)) {
            # NB: [[1]] picks out value from a list data.frame col (e.g. for formula)
            quota_f <- quota_table[i, 'quota'][[1]]
        } else {
            quota_f <- f_substitute(~if (biomass_c < cond) val else quota_f, list(
                biomass_c = biomass_c,
                cond = quota_table[i, 'biomass'],
                val = quota_table[i, 'quota'][[1]],
                quota_f = quota_f))
        }
    }

    if (!is.null(recalc_f)) {
        quota_var_name <- paste0("stock__quotafleet_", unique_action_name())
        # NB: This should remain global to model
        assign(quota_var_name, structure(
            0.0,
            desc = "Current quota of stock"))
        quota_f <- f_substitute(~(quota_var <- if (recalc_f) quota_f else quota_var), list(
            recalc_f = recalc_f,
            quota_f = quota_f,
            quota_var = as.symbol(quota_var_name)))
    }

    out <- list(
        suit_unit = "energy content",
        suit = quote( suit_f * stock_ss(stock__num) * stock_ss(stock__wgt) ),
        cons = f_substitute(
            ~quota_f * E * cur_step_size * stock_ss(predprey__suit),
            list(
                quota_f = quota_f,
                E = E)) )
    # Make sure stocks with final name are available when making up formula
    for (stock in sum_stocks) {
        assign(stock$name, stock, envir = environment(out$C))
    }
    return(out)
}

g3a_predate_catchability_predator <- function (
        prey_preferences = 1,
        energycontent = g3_parameterized('energycontent', value = 1,
            by_stock = TRUE, optimise = FALSE),
        half_feeding_f = g3_parameterized('halffeeding',
            by_predator = TRUE, optimise = FALSE),
        m0 = g3_parameterized('consumption.m0', value = 1,
            by_predator = TRUE, optimise = FALSE),
        m1 = g3_parameterized('consumption.m1', value = 0,
            by_predator = TRUE, optimise = FALSE),
        m2 = g3_parameterized('consumption.m2', value = 0,
            by_predator = TRUE, optimise = FALSE),
        m3 = g3_parameterized('consumption.m3', value = 0,
            by_predator = TRUE, optimise = FALSE),
        temperature = 0 ) {
    list(
        suit_unit = "energy content",
        suit = g3_formula(
            quote(
                (suit_f * energycontent * stock_ss(stock__num) * stock_ss(stock__wgt))^prey_preference
            ),
            energycontent = energycontent,
            prey_preference = list_to_stock_switch(prey_preferences) ),
        cons = g3_formula(
            quote(
                (stock_ss(predstock__num, vec = single) * M * psi * stock_ss(predprey__suit)) / (energycontent * total_predsuit)
            ),
            energycontent = energycontent,
            # M_L, maximum possible consumption - eqn 4.22
            M = f_substitute(quote(
                # NB: length should be the current predstock__midlen
                m0 * cur_step_size * exp(m1 * temperature - m2 * temperature^3) * predator_length^m3
            ), list(m0 = m0, m1 = m1, m2 = m2, m3 = m3, temperature = temperature)),
            # ψ_L, “feeding level” (fraction of the available food that the predator is consuming) - eqn 4.23
            psi = f_substitute(quote(
                total_predsuit / ( H * cur_step_size + total_predsuit)
            ), list(H = half_feeding_f)) ))
}

g3a_predate <- function (
        predstock,
        prey_stocks,
        suitabilities,
        catchability_f,
        overconsumption_f = quote(
            logspace_add_vec(stock__consratio * -1e3, 0.95 * -1e3) / -1e3
        ),
        run_f = ~TRUE,
        run_at = g3_action_order$predate ) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # Variables used:
    # stock__totalpredate: Biomass of total consumed (prey_stock) (prey matrix)
    # stock__consratio: Proportion of total prey biomass to be consumed, capped by overconsumption rule
    # stock__overconsumption: Single figure, proportion of total biomass consumed to total biomass hoping to be consumed. Used by g3l_understocking()
    # predprey__cons: Biomass of prey consumed by pred
    # predprey__suit: Biomass of prey suitable for consumption by pred

    # Build combined arrays for each pred/prey combination
    predpreys <- lapply(prey_stocks, function (stock) g3s_stockproduct(stock, pred = predstock) )
    names(predpreys) <- vapply(prey_stocks, function (stock) stock$name, character(1))
    predprey__conses <- lapply(predpreys, function (predprey)
        g3_stock_instance(predprey, desc = paste0("Total biomass consumption of ", predprey$name)) )
    predprey__suits <- lapply(predpreys, function (predprey)
        g3_stock_instance(predprey, desc = paste0("Suitable ", predprey$name, " by ", catchability_f$suit_unit)) )

    # Work out stock_ss(predprey__cons) call that will return entire stock__* vector
    pred_dims <- names(predpreys[[1]]$dim)[startsWith( names(predpreys[[1]]$dim), "pred_" )]
    if (length(pred_dims) == 0) {
        # Predator has no dimensions, predprey__cons has the same dimensions as prey
        cons_ss <- quote(stock_ss(predprey__cons, vec = full))
        suit_ss <- quote(stock_ss(predprey__suit, vec = full))
    } else {
        cons_ss <- call('stock_ss', as.symbol('predprey__cons'), vec = as.symbol(pred_dims[[1]]), x = as.symbol('default'))
        names(cons_ss)[[4]] <- pred_dims[[1]]
        suit_ss <- call('stock_ss', as.symbol('predprey__suit'), vec = as.symbol(pred_dims[[1]]), x = as.symbol('default'))
        names(suit_ss)[[4]] <- pred_dims[[1]]
    }

    # Build total_predsuit from all prey stocks
    total_predsuit <- 0
    for (stock in prey_stocks) {
        predprey <- predpreys[[stock$name]]
        predprey__cons <- predprey__conses[[stock$name]]
        predprey__suit <- predprey__suits[[stock$name]]
        stock__overconsumption <- structure(0.0, desc = paste0("Total overconsumption of ", stock$name))

        # NB: We're not getting the environment right, later predpreys are overriding previous. OTOH, it's not necessary so strip.
        total_predsuit <- rlang::f_rhs(g3_step(f_substitute(~total_predsuit + stock_with(stock, stock_with(predprey, 
                sum(suit_ss) )),
            list(
                suit_ss = suit_ss,
                total_predsuit = total_predsuit )), recursing = TRUE))
    }

    # For each prey stock...
    for (stock in prey_stocks) {
        stock__totalpredate <- g3_stock_instance(stock, desc = paste0("Biomass of total consumed ", stock$name, " (prey matrix)"))
        stock__consratio <- g3_stock_instance(stock, desc = paste0("Proportion of ", stock$name, " biomass to be consumed, capped by overconsumption rule"))
        stock__consconv <- g3_stock_instance(stock, desc = paste0("Conversion factor to apply ", stock$name, " overconsumption to predators"))
        predprey <- predpreys[[stock$name]]
        predprey__cons <- predprey__conses[[stock$name]]
        predprey__suit <- predprey__suits[[stock$name]]

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[step_id(run_at, 0, stock)]] <- g3_step(~{
            debug_trace("Zero total predation counter for ", stock)
            stock_with(stock, stock__totalpredate[] <- 0)
        })

        # Main predation step, iterate over prey and pull out everything this fleet needs
        out[[step_id(run_at, 1, predstock, stock, action_name)]] <- g3_step(f_substitute(~{
            debug_label("g3a_predate_fleet for ", stock)
            debug_trace("Zero ", predstock, "-", stock, " biomass-consuming counter")
            stock_with(predprey, predprey__suit[] <- 0)

            stock_iterate(stock, stock_interact(predstock, stock_with(predprey, if (run_f) {
                debug_trace("Collect all suitable ", stock, " biomass for ", predstock)
                stock_ss(predprey__suit) <- catchability_suit_f
            }), prefix = 'predator'))
        }, list(
            catchability_suit_f = f_substitute(catchability_f$suit, list(
                suit_f = list_to_stock_switch(suitabilities) )),
            run_f = run_f )))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, 2, predstock, stock, action_name)]] <- g3_step(f_substitute(~{
            debug_trace("Scale ", predstock, " catch of ", stock, " by total expected catch")
            stock_with(predprey, predprey__cons[] <- 0)
            stock_iterate(stock, stock_interact(predstock, stock_with(predprey, if (run_f) {
                # NB: In gadget2, E == wanttoeat
                stock_ss(predprey__cons) <- catchability_cons_f
                # NB: In gadget2, predprey__cons == (*cons[inarea][prey])[predl], totalpredator.cc#68
                #     All consumption is in biomass, conversion done in Prey::addNumbersConsumption
            }), prefix = 'predator'))

            # Add predstock's consumption to total consumption of stock
            stock_iterate(predstock, stock_with(stock, stock_with(predprey, {
                # NB: cons_ss may have dropped more dimensions than we've bargained for, e.g. prey_area, so may not be an exact match
                stock__totalpredate[] <- nonconform_add(stock__totalpredate[], cons_ss)
            })))
        }, list(
            catchability_cons_f = catchability_f$cons,
            cons_ss = cons_ss,
            run_f = run_f )))

        # Overconsumption: Prey adjustments
        out[[step_id(run_at, 4, stock)]] <- g3_step(f_substitute(~{
            debug_trace("Calculate ", stock, " overconsumption coefficient")
            stock_with(stock, {
                debug_trace("Apply overconsumption to ", stock)

                # NB: See prey.cc::208
                # stock__consratio == ratio
                # stock__consratio proportion of prey that we're about to consume
                stock__consratio <- overconsumption_f

                stock__overconsumption <- sum(stock__totalpredate)

                # Apply overconsumption to stock__totalpredate, work out conversion factor for predprey__cons
                stock__consconv <- 1 / avoid_zero_vec(stock__totalpredate)
                stock__totalpredate <- (stock__num * stock__wgt) * stock__consratio
                stock__overconsumption <- stock__overconsumption - sum(stock__totalpredate)
                stock__consconv <- stock__consconv * stock__totalpredate

                # Apply consumption
                stock__num <- stock__num * (1 - stock__consratio)
            })
        }, list(
            overconsumption_f = f_substitute(overconsumption_f, list(
                stock__consratio = quote(stock__totalpredate / avoid_zero_vec(stock__num * stock__wgt)) )),
            end = NULL)))

        # Overconsumption: Scale predprey_cons by the overconsumption rate
        out[[step_id(run_at, 5, predstock, stock)]] <- g3_step(f_substitute(~{
            stock_with(stock, stock_with(predprey, if (run_f) {
                debug_trace("Apply overconsumption to ", predprey, "__cons")
                # Rough equivalent fleetpreyaggregator.cc:109
                predprey__cons <- nonconform_mult(predprey__cons, stock__consconv)
            }))
        }, list(
            cons_ss = cons_ss,
            run_f = run_f )))
    }

    return(as.list(out))
}

# Wrapper for slightly less old interface
g3a_predate_fleet <- function (fleet_stock,
                                    prey_stocks,
                                    suitabilities,
                                    catchability_f,
                                    overconsumption_f = quote( logspace_add_vec(stock__consratio * -1e3, 0.95 * -1e3) / -1e3 ),
                                    run_f = ~TRUE,
                                    run_at = g3_action_order$predate) {
    predstock <- fleet_stock
    out <- g3a_predate(
        predstock = predstock,
        prey_stocks = prey_stocks,
        suitabilities = suitabilities,
        catchability_f = catchability_f,
        overconsumption_f = overconsumption_f,
        run_f = run_f,
        run_at = run_at )

    # Build combined arrays for each pred/prey combination
    predpreys <- lapply(prey_stocks, function (stock) g3s_stockproduct(stock, pred = predstock) )
    names(predpreys) <- vapply(prey_stocks, function (stock) stock$name, character(1))
    predprey__conses <- lapply(predpreys, function (predprey)
        g3_stock_instance(predprey, desc = paste0("Total biomass consumption of ", predprey$name)) )

    # Work out stock_ss(predprey__cons) call that will return entire stock__* vector
    pred_dims <- names(predpreys[[1]]$dim)[startsWith( names(predpreys[[1]]$dim), "pred_" )]
    if (length(pred_dims) == 0) {
        # Predator has no dimensions, predprey__cons has the same dimensions as prey
        cons_ss <- quote(stock_ss(predprey__cons, vec = full))
        suit_ss <- quote(stock_ss(predprey__suit, vec = full))
    } else {
        cons_ss <- call('stock_ss', as.symbol('predprey__cons'), vec = as.symbol(pred_dims[[1]]), x = as.symbol('default'))
        names(cons_ss)[[4]] <- pred_dims[[1]]
        suit_ss <- call('stock_ss', as.symbol('predprey__suit'), vec = as.symbol(pred_dims[[1]]), x = as.symbol('default'))
        names(suit_ss)[[4]] <- pred_dims[[1]]
    }

    for (stock in prey_stocks) {
        predprey <- predpreys[[stock$name]]
        predprey__cons <- predprey__conses[[stock$name]]

        # Generate stock__predby_predstock for backwards compatibility
        predstock_var <- as.symbol(paste0('stock__predby_', predstock$name))
        assign(as.character(predstock_var), g3_stock_instance(stock, desc = paste0("Total biomass of ", stock$name, " captured by ", predstock$name)))

        out[[step_id(run_at, 99, predstock, stock)]] <- g3_step(f_substitute(~{
            stock_with(stock, stock__predby_predstock[] <- 0)
            stock_iterate(predstock, stock_with(stock, stock_with(predprey, {
                # NB: cons_ss may have dropped more dimensions than we've bargained for, e.g. prey_area, so may not be an exact match
                stock__predby_predstock[] <- nonconform_add(stock__predby_predstock, cons_ss)
            })))
        }, list(
            cons_ss = cons_ss,
            stock__predby_predstock = predstock_var,
            run_f = run_f )))
    }
    return(out)
}

# Wrapper for old interface
g3a_predate_totalfleet <- function (fleet_stock,
                                    prey_stocks,
                                    suitabilities,
                                    amount_f,
                                    overconsumption_f = quote( logspace_add_vec(stock__consratio * -1e3, 0.95 * -1e3) / -1e3 ),
                                    run_f = ~TRUE,
                                    run_at = g3_action_order$predate) {
    g3a_predate_fleet(fleet_stock, prey_stocks, suitabilities, overconsumption_f = overconsumption_f, run_f = run_f, run_at = run_at,
        catchability_f = g3a_predate_catchability_totalfleet(amount_f))
}
