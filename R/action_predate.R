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
            # NB: Divide by cur_step_size, assuming E is per-year
            ~E * cur_step_size * stock_ss(predprey__suit),
            list(E = E)) )
}

g3a_predate_catchability_project <- function (
    quota_f = NULL,
    landings_f = NULL,
    # Proportion of quota assigned to predator
    quota_prop = g3_parameterized("quota.prop", by_predator = TRUE, value = 1),
    # Proportion of predator yearly quota assigned to which step
    cons_step = g3_parameterized("cons.step", by_predator = TRUE, by_step = TRUE,
        value = quote( step_lengths / 12.0 )),
    unit = c("biomass", "biomass-year", "harvest-rate", "harvest-rate-year",
             "individuals", "individuals-year") ) {

    # Unit landings_f is expressed in
    landings_unit <- match.arg(unit)
    # Unit quota_f is expressed in
    quota_unit <- match.arg(attr(quota_f, "catchability_unit"), eval(formals()$unit))
    # Unit suitability / total_predsuit will be expressed in
    suit_unit <- if (startsWith(landings_unit, "individuals")) "individuals" else "biomass"

    # Take proportion of quota based on quota_prop
    if (!is.null(quota_f)) quota_f <- f_substitute(quote( quota_prop * quota_f ), list(
        quota_prop = list_to_stock_switch(quota_prop, "predstock"),
        quota_f = quota_f ))

    # For both quota_f & landings_f, divide by total_predsuit & cons_step as appropriate
    adapt <- function (f, unit) {
        if (is.null(f)) return(f)

        # if f returns biomass / individuals
        if (startsWith(unit, "biomass")) {
            if (suit_unit == "individuals") stop("Cannot use a quota in biomass with landings in individuals")
            f <- f_substitute(quote( f / total_predsuit ), list(f = f))
        }
        if (startsWith(unit, "individuals")) {
            if (suit_unit == "biomass") stop("Cannot use a quota in individuals with landings in biomass")
            f <- f_substitute(quote( f / total_predsuit ), list(f = f))
        }
        if (endsWith(unit, "-year")) {
            f <- f_substitute(quote( f * cons_step ), list(f = f, cons_step = cons_step))
        }
        return(f)
    }
    quota_f <- adapt(quota_f, quota_unit)
    landings_f <- adapt(landings_f, landings_unit)

    # Combine quota/landings into a single formula
    if (is.null(quota_f)) {
        combined_f <- landings_f
    } else if (is.null(landings_f)) {
        combined_f <- quota_f
    } else {
        combined_f <- f_substitute(
            quote( if (cur_year_projection) quota_f else landings_f ),
            list(
                landings_f = landings_f,
                quota_f = quota_f ))
    }

    # Return catchablity based on suit_unit
    if (suit_unit == "biomass") {
        list(
            suit_unit = "total biomass",
            suit = quote( suit_f * stock_ss(stock__num) * stock_ss(stock__wgt) ),
            cons = f_substitute(
                quote( stock_ss(predprey__suit) * combined_f ),
                list(combined_f = combined_f)) )
    } else if (suit_unit == "individuals") {
        list(
            suit_unit = "number of individuals",
            suit = quote( suit_f * stock_ss(stock__num) ),
            cons = f_substitute(
                quote( stock_ss(predprey__suit) * combined_f * stock_ss(stock__wgt) ),
                list(combined_f = combined_f)) )
    } else stop("Unknown suitability unit ", suit_unit)
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
        suit_unit = "total biomass",
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

g3a_predate_maxconsumption <- function (
        m0 = g3_parameterized('consumption.m0', value = 1,
            by_predator = TRUE, optimise = FALSE),
        m1 = g3_parameterized('consumption.m1', value = 0,
            by_predator = TRUE, optimise = FALSE),
        m2 = g3_parameterized('consumption.m2', value = 0,
            by_predator = TRUE, optimise = FALSE),
        m3 = g3_parameterized('consumption.m3', value = 0,
            by_predator = TRUE, optimise = FALSE),
        temperature = 0 ) {
    # M_L, maximum possible consumption - eqn 4.22
    f_substitute(quote(
        # NB: length should be the current predstock__midlen
        m0 * cur_step_size * exp(m1 * temperature - m2 * temperature^3) * predator_length^m3
    ), list(m0 = m0, m1 = m1, m2 = m2, m3 = m3, temperature = temperature))
}

g3a_predate_catchability_predator <- function (
        prey_preferences = 1,
        energycontent = g3_parameterized('energycontent', value = 1,
            by_stock = by_stock, optimise = FALSE),
        half_feeding_f = g3_parameterized('halffeeding',
            by_predator = by_predator, optimise = FALSE),
        max_consumption = g3a_predate_maxconsumption(temperature = temperature),
        temperature = 0,
        by_predator = TRUE,
        by_stock = TRUE ) {
    # https://github.com/gadget-framework/gadget2/blob/master/src/stockpredator.cc#L144
    list(
        suit_unit = "energy content",
        suit = g3_formula(
            quote(
                (suit_f * energycontent * stock_ss(stock__num) * stock_ss(stock__wgt))^prey_preference
            ),
            energycontent = energycontent,
            prey_preference = list_to_stock_switch(prey_preferences) ),
        feeding_level = g3_formula(
            # ψ_L, “feeding level” (fraction of the available food that the predator is consuming) - eqn 4.23
            quote(
                total_predsuit / ( half_feeding * cur_step_size + total_predsuit )
            ),
            half_feeding = half_feeding_f ),
        cons = g3_formula(
            quote(
                (stock_ss(predstock__num, vec = single) * max_consumption * feeding_level * stock_ss(predprey__suit)) / (energycontent * total_predsuit)
            ),
            energycontent = energycontent,
            max_consumption = max_consumption ))
}

g3a_predate <- function (
        predstock,
        prey_stocks,
        suitabilities,
        catchability_f,
        overconsumption_f = quote( dif_pmin(stock__consratio, 0.95, 1e3) ),
        run_f = ~TRUE,
        run_at = g3_action_order$predate ) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # If predstock is a list, run g3a_predate against everything in turn
    if (!g3_is_stock(predstock) && is.list(predstock)) {
        attributes(predstock) <- NULL  # Can't have names polluting the ordering
        return(g3_collate(lapply(predstock, function (s) g3a_predate(
            s,
            prey_stocks,
            suitabilities,
            catchability_f,
            overconsumption_f = overconsumption_f,
            run_f = run_f,
            run_at = run_at ))))
    }

    stopifnot(g3_is_stock(predstock))
    stopifnot(is.list(prey_stocks) && all(sapply(prey_stocks, g3_is_stock)))

    # Variables used:
    # stock__totalpredate: Biomass of total consumed (prey_stock) (prey matrix)
    # stock__consratio: Proportion of total prey biomass to be consumed, capped by overconsumption rule
    # stock__overconsumption: Single figure, proportion of total biomass consumed to total biomass hoping to be consumed. Used by g3l_understocking()
    # predstock__totalsuit: Total prey suitable for consumption by pred
    # predprey__cons: Biomass of prey consumed by pred
    # predprey__suit: Biomass/number of prey suitable for consumption by pred

    predstock__feedinglevel <- g3_stock_instance(predstock, desc = "Fraction of the available food that the predator is consuming")
    predstock__totalsuit <- g3_stock_instance(predstock, desc = paste0("Total suitable prey by ", catchability_f$suit_unit))

    # Build combined arrays for each pred/prey combination
    predpreys <- lapply(prey_stocks, function (stock) g3s_stockproduct(stock, predator = predstock, ignore_dims = c('predator_area')) )
    names(predpreys) <- vapply(prey_stocks, function (stock) stock$name, character(1))
    predprey__conses <- lapply(predpreys, function (predprey)
        g3_stock_instance(predprey, desc = paste0("Total biomass consumption of ", predprey$name)) )
    predprey__suits <- lapply(predpreys, function (predprey)
        g3_stock_instance(predprey, desc = paste0("Suitable ", predprey$name, " by ", catchability_f$suit_unit)) )

    # Work out stock_ss(predprey__cons) call that will return entire stock__* vector
    pred_dims <- names(predpreys[[1]]$dim)[startsWith( names(predpreys[[1]]$dim), "predator_" )]
    if (length(pred_dims) == 0) {
        # Predator has no dimensions, predprey__cons has the same dimensions as prey
        cons_ss <- quote(stock_ss(predprey__cons, vec = full))
    } else {
        cons_ss <- call('stock_ss', as.symbol('predprey__cons'), vec = as.symbol(pred_dims[[1]]), x = as.symbol('default'))
        names(cons_ss)[[4]] <- pred_dims[[1]]
    }

    # For each prey stock...
    for (stock in prey_stocks) {
        stock__totalpredate <- g3_stock_instance(stock, desc = paste0("Biomass of total consumed ", stock$name, " (prey matrix)"))
        stock__consratio <- g3_stock_instance(stock, desc = paste0("Proportion of ", stock$name, " biomass to be consumed, capped by overconsumption rule"))
        stock__consconv <- g3_stock_instance(stock, desc = paste0("Conversion factor to apply ", stock$name, " overconsumption to predators"))
        stock__overconsumption <- structure(0.0, desc = paste0("Total overconsumption of ", stock$name))
        predprey <- predpreys[[stock$name]]
        predprey__cons <- predprey__conses[[stock$name]]
        predprey__suit <- predprey__suits[[stock$name]]
        predprey__dynlen <- g3_stock_instance(predprey, desc = "Mean length")
        predprey__dynlensd <- g3_stock_instance(predprey, desc = "Std.dev. of length")

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[step_id(run_at, "g3a_predate", 0, 0, stock)]] <- g3_step(~{
            stock_with(stock, stock__totalpredate[] <- 0)
        })

        # NB: A stock might be a predstock elsewhere, so give this a separate ID.
        out[[step_id(run_at, "g3a_predate", 0, 1, predstock)]] <- g3_step(~{
            stock_with(predstock, predstock__totalsuit[] <- 0)
        })

        # Generate suitability reports for this stock
        if (!("dynlen" %in% names(stock$dim))) {
            suitrep_step <- g3a_suitability_report(
                predstock,
                stock,
                resolve_stock_list(suitabilities, stock) )
            # Add suitability report steps to our list
            for (i in seq_along(suitrep_step)) out[[names(suitrep_step)[[i]]]] <- suitrep_step[[i]]
        }

        # Main predation step, iterate over prey and pull out everything this fleet needs
        catchability <- f_substitute(catchability_f$suit, list(suit_f = quote(suitability)))
        environment(catchability)$suitability <- list_to_stock_switch(suitabilities)

        out[[step_id(run_at, "g3a_predate", 1, predstock, stock, action_name)]] <- g3_step(f_substitute(~{
            debug_label("g3a_predate for ", predstock, " predating ", stock)
            stock_with(predprey, predprey__suit[] <- 0)

            stock_iterate(stock, stock_interact(predstock, stock_with(predprey, if (run_f) {
                debug_trace("Collect all suitable ", stock, " biomass for ", predstock)
                if (stock_hasdim(stock, "dynlen")) {
                    # TODO: Better subsetting?
                    stock_ss(predprey__dynlen) <- stock_ss(stock__dynlen)
                    stock_ss(predprey__dynlensd) <- stock_ss(stock__dynlensd)
                }
                stock_ss(predprey__suit) <- catchability
                stock_ss(predstock__totalsuit, vec = single) <- stock_ss(predstock__totalsuit, vec = single) + sum(stock_ss(predprey__suit))
            }), prefix = 'predator'))
        }, list(
            run_f = run_f )))

        # Add dependent formulas to catchability, let g3_step work out where they go
        environment(catchability_f$cons)$total_predsuit <- ~stock_with(predstock, stock_ss(predstock__totalsuit, vec = single))
        if (!is.null(catchability_f$feeding_level)) {
            environment(catchability_f$cons)$feeding_level <- f_substitute(quote(
                stock_ss(predstock__feedinglevel, vec = single) <- feeding_level_f
            ), list(
                # NB: Dependency mismatch, bodge around for now
                feeding_level_f = catchability_f$feeding_level,
                end = NULL ))
            environment(environment(catchability_f$cons)$feeding_level)$predstock__feedinglevel <- predstock__feedinglevel
        }

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, "g3a_predate", 2, predstock, stock, action_name)]] <- g3_step(f_substitute(~{
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
        out[[step_id(run_at, "g3a_predate", 4, stock)]] <- g3_step(f_substitute(~{
            debug_trace("Calculate ", stock, " overconsumption coefficient")
            stock_with(stock, {
                debug_trace("Apply overconsumption to ", stock)

                # NB: See prey.cc::208
                # stock__consratio == ratio
                # stock__consratio proportion of prey that we're about to consume
                stock__consratio <- stock__totalpredate / avoid_zero(stock__num * stock__wgt)
                stock__consratio <- overconsumption_f

                # stock__overconsumption: biomass over overconsumption limit.  This is used by g3l_understocking()
                stock__overconsumption <- sum(stock__totalpredate)

                # Apply overconsumption to stock__totalpredate, work out conversion factor for predprey__cons
                stock__consconv <- 1 / avoid_zero(stock__totalpredate)
                stock__totalpredate <- (stock__num * stock__wgt) * stock__consratio
                stock__overconsumption <- stock__overconsumption - sum(stock__totalpredate)
                stock__consconv <- stock__consconv * stock__totalpredate

                # Apply consumption
                stock__num <- stock__num * (1 - stock__consratio)
            })
        }, list(
            overconsumption_f = overconsumption_f,
            end = NULL)))

        # Overconsumption: Scale predprey_cons by the overconsumption rate
        out[[step_id(run_at, "g3a_predate", 5, predstock, stock)]] <- g3_step(f_substitute(~{
            stock_with(stock, stock_with(predprey, if (run_f) {
                debug_trace("Apply overconsumption to ", predprey, "__cons")
                # Rough equivalent fleetpreyaggregator.cc:109
                predprey__cons <- nonconform_mult(predprey__cons, stock__consconv)
            }))
        }, list(
            run_f = run_f )))
    }

    return(as.list(out))
}

# Wrapper for slightly less old interface
g3a_predate_fleet <- function (fleet_stock,
                                    prey_stocks,
                                    suitabilities,
                                    catchability_f,
                                    overconsumption_f = quote( dif_pmin(stock__consratio, 0.95, 1e3) ),
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
    predpreys <- lapply(prey_stocks, function (stock) g3s_stockproduct(stock, predator = predstock, ignore_dims = c('predator_area')) )
    names(predpreys) <- vapply(prey_stocks, function (stock) stock$name, character(1))
    predprey__conses <- lapply(predpreys, function (predprey)
        g3_stock_instance(predprey, desc = paste0("Total biomass consumption of ", predprey$name)) )

    # Work out stock_ss(predprey__cons) call that will return entire stock__* vector
    pred_dims <- names(predpreys[[1]]$dim)[startsWith( names(predpreys[[1]]$dim), "predator_" )]
    if (length(pred_dims) == 0) {
        # Predator has no dimensions, predprey__cons has the same dimensions as prey
        cons_ss <- quote(stock_ss(predprey__cons, vec = full))
    } else {
        cons_ss <- call('stock_ss', as.symbol('predprey__cons'), vec = as.symbol(pred_dims[[1]]), x = as.symbol('default'))
        names(cons_ss)[[4]] <- pred_dims[[1]]
    }

    for (stock in prey_stocks) {
        predprey <- predpreys[[stock$name]]
        predprey__cons <- predprey__conses[[stock$name]]

        # Generate stock__predby_predstock for backwards compatibility
        predstock_var <- as.symbol(paste0('stock__predby_', predstock$name))
        assign(as.character(predstock_var), g3_stock_instance(stock, desc = paste0("Total biomass of ", stock$name, " captured by ", predstock$name)))

        out[[step_id(run_at, "g3a_predate", 99, predstock, stock)]] <- g3_step(f_substitute(~{
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
                                    overconsumption_f = quote( dif_pmin(stock__consratio, 0.95, 1e3) ),
                                    run_f = ~TRUE,
                                    run_at = g3_action_order$predate) {
    g3a_predate_fleet(fleet_stock, prey_stocks, suitabilities, overconsumption_f = overconsumption_f, run_f = run_f, run_at = run_at,
        catchability_f = g3a_predate_catchability_totalfleet(amount_f))
}
