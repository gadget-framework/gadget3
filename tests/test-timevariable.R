library(magrittr)
library(unittest)

library(gadget3)

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

# Check a lookup with a single value in it still works
single_lookup <- gadget3:::g3_intlookup('single_lookup', c(1), c(100))
single_lookup_rv_1 <- 0
single_lookup_rv_2 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('single_lookup')
    single_lookup_rv_1 <- lookup_rv_1_f
    single_lookup_rv_2 <- lookup_rv_2_f
    g3_report(single_lookup_rv_1)
    g3_report(single_lookup_rv_2)
}, list(
    lookup_rv_1_f = single_lookup('getdefault', ~1, 99),
    lookup_rv_2_f = single_lookup('getdefault', ~2, 99))))
expecteds$single_lookup_rv_1 <- 100
expecteds$single_lookup_rv_2 <- 99

# Single-area form works as expected
single_area_lookup <- g3_timeareadata('single_area_lookup', read.table(header = TRUE, text = "
year	step	area	total_weight
1983	1	1	198311
1983	2	1	198321
1984	1	1	198411
"))

single_area_1 <- 0
single_area_2 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('single_area_lookup')
    cur_year <- 1983 ; cur_step <- 1 ; area <- 1
    single_area_1 <- lookup_f
    g3_report(single_area_1)
    cur_year <- 1983 ; cur_step <- 1 ; area <- 2
    single_area_2 <- lookup_f
    g3_report(single_area_2)
}, list(lookup_f = single_area_lookup)))
expecteds$single_area_1 <- 198311
expecteds$single_area_2 <- 0

# no-area form works as expected
no_area_lookup <- g3_timeareadata('no_area_lookup', read.table(header = TRUE, text = "
year	step	total_weight
1983	1	198311
1983	2	198321
1984	1	198411
"))

no_area_1 <- 0
no_area_2 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('no_area_lookup')
    cur_year <- 1983 ; cur_step <- 1 ; area <- 1
    no_area_1 <- lookup_f
    g3_report(no_area_1)
    cur_year <- 1983 ; cur_step <- 2 ; area <- 2
    no_area_2 <- lookup_f
    g3_report(no_area_2)
}, list(lookup_f = no_area_lookup)))
expecteds$no_area_1 <- 198311
expecteds$no_area_2 <- 198321

# no-step
no_step_lookup <- g3_timeareadata('no_step_lookup', read.table(header = TRUE, text = "
year	area	total_weight
1983	1	19831
1983	2	19832
1984	3	19843
"))

no_step_1 <- 0
no_step_2 <- 0
no_step_3 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('no_step_lookup')
    cur_year <- 1984 ; cur_step <- 1 ; area <- 3
    no_step_1 <- lookup_f
    g3_report(no_step_1)
    cur_year <- 1984 ; cur_step <- 2 ; area <- 3
    no_step_2 <- lookup_f
    g3_report(no_step_2)
    cur_year <- 1984 ; cur_step <- 3 ; area <- 2
    no_step_2 <- lookup_f
    g3_report(no_step_3)
}, list(lookup_f = no_step_lookup)))
expecteds$no_step_1 <- 19843
expecteds$no_step_2 <- 19843
expecteds$no_step_3 <- 0

###############################################################################

actions <- c(actions, ~{
    comment('done')
    nll <- nll + g3_param('rv')
    return(nll)
})
params <- list(rv=0)

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

# Compare everything we've been told to compare
result <- model_fn(params)
# str(attributes(result), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(attr(result, n), expecteds[[n]]), n)
}
param_template <- attr(model_cpp, "parameter_template")
param_template$value <- params[param_template$switch]
gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
