library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[names(environment(model_cpp)$model_parameters)])
        model_tmb_report <- model_tmb$report(par)
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(environment(model_fn)$model_report[[n]]),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

matrix_vec_out <- rep(0, 5)
actions <- list(~{
    matrix_vec_out <- g3_matrix_vec(g3_param_array('matrix_vec_tf'), g3_param_vector('matrix_vec_vec'))
    g3_report(matrix_vec_out)
    
    return(g3_param('x'))
})
params <- list(
    matrix_vec_vec = c(10, 100, 1000),
    matrix_vec_tf = matrix(c(0,1,0,1,0,0,0,0,1), nrow = 3),
    x=1.0)

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("g3_matrix_vec", {
    params <- list(
        matrix_vec_vec = c(10, 100, 1000),
        matrix_vec_tf = array(c(0,1,0, 1,0,0, 0,0,1), dim = c(3,3)),
        x=1.0)
    result <- model_fn(params)
    r <- environment(model_fn)$model_report
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(
        r$matrix_vec_out,
        c(100, 10, 1000)), "matrix_vec_out: Vector transformed, 1 dimensional again")

    tmb_r_compare(model_fn, model_tmb, params)
})
