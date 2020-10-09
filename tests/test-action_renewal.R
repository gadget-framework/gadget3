library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[attr(model_cpp, 'parameter_template')$switch])
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

areas <- list(a=1, b=2, c=3, d=4)
stock_a <- g3_stock('stock_a', seq(10, 10, 5)) %>% g3s_livesonareas(areas[c('a')])
stock_ac <- g3_stock('stock_ac', seq(10, 10, 5)) %>% g3s_livesonareas(areas[c('a', 'c')])
    
cur_time <- 0L  # Initialconditions needs to know what the time is
actions <- list(
    g3a_initialconditions(stock_a, ~area * 100 + stock_a__minlen, ~stock_a__minlen + 100),
    g3a_initialconditions(stock_ac, ~area * 1000 + stock_ac__minlen, ~stock_a__minlen + 200),
    list(
        '999' = ~{
            g3_report(stock_a__num)
            g3_report(stock_ac__num)
            g3_report(stock_a__wgt)
            g3_report(stock_ac__wgt)
            return(g3_param('x'))
        }))
params <- list(x=1.0)
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

result <- model_fn(params)

# Populated numbers
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$stock_a__num),
    c(110)), "stock_a__num populated")
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$stock_ac__num),
    c(1010, 3010)), "stock_ac__num populated")

# Populated mean weights
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$stock_a__wgt),
    c(110)), "stock_a__wgt populated")
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$stock_ac__wgt),
    c(210, 210)), "stock_ac__wgt populated")

tmb_r_compare(model_fn, model_tmb, params)
