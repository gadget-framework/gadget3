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
                model_tmb_report[[n]],
                environment(model_fn)$model_report[[n]],
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

actions <- list()
expecteds <- new.env(parent = emptyenv())

###############################################################################

area <- 1L
cur_step <- 1L
cur_year <- 1980L

tad_lookup <- g3_timeareadata('tad', read.table(header = TRUE, text = "
year	step	area	total_weight
1983	1	1	198311
1983	2	1	198321
1984	1	1	198411
1984	2	1	198421
1983	1	2	198312
1983	2	2	198322
1984	1	2	198412
1984	2	2	198422
"))

tad_get_1 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('tad_get_1')
    cur_year <- 1983
    cur_step <- 1
    area <- 1
    tad_get_1 <- lookup_f
    g3_report(tad_get_1)
}, list(lookup_f = tad_lookup)))
expecteds$tad_get_1 <- 198311

tad_get_2 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('tad_get_2')
    cur_year <- 1984
    cur_step <- 2
    area <- 1
    tad_get_2 <- lookup_f
    g3_report(tad_get_2)
}, list(lookup_f = tad_lookup)))
expecteds$tad_get_2 <- 198421

tad_get_3 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('tad_get_3')
    cur_year <- 2008  # NB: 2008 not in table
    cur_step <- 2
    area <- 1
    tad_get_3 <- lookup_f
    g3_report(tad_get_3)
}, list(lookup_f = tad_lookup)))
expecteds$tad_get_3 <- 0  # i.e. missing value

###############################################################################

actions <- c(actions, ~{
    comment('done')
    return(g3_param('rv'))
})
params <- list(rv=0)

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

# Compare everything we've been told to compare
result <- model_fn(params)
# str(as.list(environment(model_fn)$model_report), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(environment(model_fn)$model_report[[n]], expecteds[[n]]), n)
}
tmb_r_compare(model_fn, model_tmb, params)
