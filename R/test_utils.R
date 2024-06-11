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

# Re-implementation that can handle changing non-optimised parameters
ut_tmb_r_compare2 <- function (
        model_fn,
        model_cpp,
        params ) {
    dearray <- function (x) {
        # TMB Will produce 0/1 for TRUE/FALSE
        if (is.logical(x)) {
            oldattr <- attributes(x)
            x <- as.numeric(x)
            attributes(x) <- oldattr  # Preserve arrayness
        }
        return(x)
    }

    if (!nzchar(Sys.getenv('G3_TEST_TMB'))) {
        writeLines("# skip: not running TMB tests")
        return()
    }

    # Splice R parameters into parameter_template
    param_template <- attr(model_cpp, 'parameter_template')
    param_template$value[names(params)] <- params

    # writeLines(TMB::gdbsource(g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"), output_script = TRUE)))
    model_tmb <- g3_tmb_adfun(model_cpp, param_template, compile_flags = c("-O0", "-g"))

    model_tmb_report <- model_tmb$report()
    r_result <- model_fn(params)

    for (n in names(attributes(r_result))) {
        unittest::ok(unittest::ut_cmp_equal(
            dearray(model_tmb_report[[n]]),
            dearray(attr(r_result, n)),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
}

ut_cmp_code <- function(a, b, optimize = FALSE) {
    if (rlang::is_formula(a)) a <- rlang::f_rhs(a)
    if (rlang::is_formula(b)) b <- rlang::f_rhs(b)
    attr(a, "srcref") <- NULL
    attr(a, "srcfile") <- NULL
    attr(a, "wholeSrcref") <- NULL
    attr(b, "srcref") <- NULL
    attr(b, "srcfile") <- NULL
    attr(b, "wholeSrcref") <- NULL
    if (isTRUE(optimize)) {
        a <- f_optimize(a)
        b <- f_optimize(b)
    }
    unittest::ut_cmp_identical(a, b)
}

# Compare array by turning it back into a table first
ut_cmp_array <- function (ar, table_text, ...) {
    df <- as.data.frame.table(ar, stringsAsFactors = FALSE)
    tbl <- utils::read.table(
        header = TRUE,
        stringsAsFactors = FALSE,
        colClasses = sapply(df, class),
        text = table_text)
    rownames(tbl) <- NULL
    unittest::ut_cmp_equal(df, tbl, ...)
}

ut_cmp_df <- function (df, table_text, ...) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    if (nzchar(trimws(table_text))) {
        expected <- utils::read.table(
            header = TRUE,
            check.names = FALSE,
            stringsAsFactors = FALSE,
            colClasses = sapply(df, class),
            text = table_text)
    } else {
        # Empty data frame, so we can copy expected values
        expected <- df[c(),, drop = FALSE]
    }
    unittest::ut_cmp_equal(df, expected, ...)
}
