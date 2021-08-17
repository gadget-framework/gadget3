# Helpers for unit testing, not for general use

# Compare output of TMB & R model runs
ut_tmb_r_compare <- function (model_fn, model_tmb, param_template) {
    dearray <- function (x) {
        # TMB Won't produce arrays for 1-dimensional arrays, so moosh down R correspondingly
        if (is.array(x) && length(dim(x)) == 1) return(as.vector(x))
        # TMB Will produce 0/1 for TRUE/FALSE
        if (is.logical(x)) return(as.numeric(x))
        # TMB Won't have array labels
        names(dim(x)) <- NULL
        return(unname(x))
    }

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb_report <- model_tmb$report(g3_tmb_par(param_template))
        r_result <- model_fn(param_template$value)
        for (n in names(attributes(r_result))) {
            unittest::ok(unittest::ut_cmp_equal(
                model_tmb_report[[n]],
                dearray(attr(r_result, n)),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}
