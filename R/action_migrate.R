g3a_migrate_normalize <- function(row_total = 1) {
    g3a_migrate_normalize <- g3_native(function (vec, src_idx, row_total) {
        vec[[src_idx]] <- 0  # Clear no-movement part of array
        vec <- vec ** 2  # ~abs(x) for all entries
        vec[[src_idx]] <- row_total - sum(vec)  # No-movement part should balance other entries
        vec <- vec / row_total  # Normalise rest
        return(vec)
    }, cpp = '[](vector<Type> vec, int src_idx, double row_total) {
        vec(src_idx) = 0;
        vec = pow(vec, (Type)2);
        vec(src_idx) = row_total - vec.sum();
        vec /= row_total;
        return vec;
    }')

    f_substitute(~g3a_migrate_normalize(stock__migratematrix[,stock__area_idx], stock__area_idx, row_total), list(
        row_total = row_total))
}


g3a_migrate <- function(
        stock,
        migrate_f,
        normalize_f = g3a_migrate_normalize(),
        run_f = TRUE,
        run_at = 2) {
    stopifnot(g3_is_stock(stock))
    stopifnot(rlang::is_formula(migrate_f))

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)
    stock__migratematrix <- structure(
        array(
            dim = list(dest_area = stock$dim$area, area = stock$dim$area),
            dimnames = list(dest_area = stock$dimnames$area, area = stock$dimnames$area)),
        desc = "Migration matrix")
    # NB: This is global to force assignment to be by value: g3_with(x, stock_ss(stock__num)) will store a reference.
    stock__premigrate <- structure(
        array(dim = stock$dim[[1]]),
        desc = "Temporary copy of stock numbers for migration")

    out <- list()
    action_name <- unique_action_name()

    # NB: Do this once for all possible migrate actions for stock
    out[[step_id(run_at, stock, 1)]] <- g3_step(f_substitute(~{
        debug_trace("Clear ", stock, " migration matrix")
        stock_with(stock, stock__migratematrix[] <- NaN)
    }, list()))

    out[[step_id(run_at, stock, 2, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_migrate: Migration of ", stock)
        
        stock_iterate(stock, if (run_f) {
            debug_trace("Fill in any gaps in the migration matrix")
            if (any(is.nan(stock__migratematrix[,stock__area_idx]))) {
                for (stock__destarea_idx in seq_along(stock__areas)) g3_with(dest_area := stock__areas[[stock__destarea_idx]], {
                    stock__migratematrix[stock__destarea_idx, stock__area_idx] <- migrate_f
                })
                stock__migratematrix[,stock__area_idx] <- normalize_f
            }

            debug_trace("Apply migration matrix to current stock")
            stock__premigrate <- stock_ss(stock__num)
            stock_ss(stock__num) <- 0  # Temporarily remove stock that we're redistributing
            for (stock__destarea_idx in seq_along(stock__areas)) g3_with(dest_area := stock__areas[[stock__destarea_idx]], {
                stock_ss(stock__num, area = stock__destarea_idx) <-
                    stock_ss(stock__num, area = stock__destarea_idx) +
                    stock__migratematrix[stock__destarea_idx, stock__area_idx] * stock__premigrate
            })
        })
    }, list(
        migrate_f = migrate_f,
        normalize_f = normalize_f,
        run_f = run_f)))
    return(out)
}
