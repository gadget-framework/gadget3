# Helpers for unit testing, not for general use

# Compare output of TMB & R model runs
ut_tmb_r_compare <- function (model_fn, model_tmb, param_template, ignore_dimname = 'time') {
    dearray <- function (x) {
        # TMB Won't produce arrays for 1-dimensional arrays, so moosh down R correspondingly
        if (is.array(x) && length(dim(x)) == 1) x <- as.vector(x)
        # TMB Will produce 0/1 for TRUE/FALSE
        if (is.logical(x)) x <- as.numeric(x)
        # TMB can't produce dynamic dimnames
        if (is.array(x) && ignore_dimname %in% names(dimnames(x))) {
            dimnames(x)[[ignore_dimname]] <- seq_along(dimnames(x)[[ignore_dimname]])
        }
        return(x)
    }

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb_report <- model_tmb$report(g3_tmb_par(param_template, include_random = TRUE))
        r_result <- model_fn(param_template$value)
        for (n in names(attributes(r_result))) {
            unittest::ok(unittest::ut_cmp_equal(
                dearray(model_tmb_report[[n]]),
                dearray(attr(r_result, n)),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}
