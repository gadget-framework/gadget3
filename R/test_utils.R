# Helpers for unit testing, not for general use

# Compare output of TMB & R model runs
ut_tmb_r_compare <- function (model_fn, model_tmb, param_template, model_cpp = NULL) {
    dearray <- function (x) {
        # TMB Will produce 0/1 for TRUE/FALSE
        if (is.logical(x)) {
            oldattr <- attributes(x)
            x <- as.numeric(x)
            attributes(x) <- oldattr  # Preserve arrayness
        }
        return(x)
    }

    if (!is.data.frame(param_template)) {
        if (is.null(model_cpp)) stop("Provide model_cpp if param_template is a list")
        pt <- attr(model_cpp, 'parameter_template')
        pt$value <- param_template
        param_template <- pt
    }

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb_report <- model_tmb$report(g3_tmb_par(param_template))
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
