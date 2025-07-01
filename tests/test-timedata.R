library(magrittr)
library(unittest)

library(gadget3)

actions <- list()
expecteds <- new.env(parent = emptyenv())

# Do some tests directly first
do_lookup <- function(df, cur_vals) {
    # Add a derived total_weight field, with values pasted together
    df$total_weight <- do.call(paste, df)
    lookup_f <- g3_timeareadata('l', df)
    # Bodge g3_global first
    environment(lookup_f)$l <- g3_eval(attr(environment(lookup_f)$l, 'g3_global_init_val'))
    g3_eval(lookup_f, cur_vals)
}

ok(ut_cmp_identical(
    do_lookup(expand.grid(age=1:3, area=4), list(age=3, area=3)),
    0), "age/area: Outside area, no match")
ok(ut_cmp_identical(
    do_lookup(expand.grid(age=1:3, area=4), list(age=3, area=4)),
    "3 4"), "age/area: Inside area")
ok(ut_cmp_identical(
    do_lookup(expand.grid(age=1:3, year=2000:2004, step=1:2), list(age=3, cur_year=2000, cur_step=2)),
    "3 2000 2"), "age/year/step matches")

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

tad_lookup_1 <- 0
tad_lookup_2 <- 0
tad_lookup_3 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('tad_lookup')
    cur_year <- 1983 ; cur_step <- 1 ; area <- 1
    tad_lookup_1 <- lookup_f
    REPORT(tad_lookup_1)

    cur_year <- 1984 ; cur_step <- 2 ; area <- 1
    tad_lookup_2 <- lookup_f
    REPORT(tad_lookup_2)

    # NB: 2008 not in table
    cur_year <- 2008 ; cur_step <- 2 ; area <- 1
    tad_lookup_3 <- lookup_f
    REPORT(tad_lookup_3)

}, list(lookup_f = tad_lookup)))
expecteds$tad_lookup_1 <- 198311
expecteds$tad_lookup_2 <- 198421
expecteds$tad_lookup_3 <- 0

# Check a lookup with a single value in it still works
single_lookup_gen <- gadget3:::g3_intlookup('single_lookup', c(1), c(100))
single_lookup_rv_1 <- 0
single_lookup_rv_2 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('single_lookup')
    single_lookup_rv_1 <- lookup_rv_1_f
    single_lookup_rv_2 <- lookup_rv_2_f
    REPORT(single_lookup_rv_1)
    REPORT(single_lookup_rv_2)
}, list(
    lookup_rv_1_f = single_lookup_gen('getdefault', ~1, 99),
    lookup_rv_2_f = single_lookup_gen('getdefault', ~2, 99))))
expecteds$single_lookup_rv_1 <- 100
expecteds$single_lookup_rv_2 <- 99

# Single-area form works as expected
single_area_lookup_tad <- g3_timeareadata('single_area_lookup', read.table(header = TRUE, text = "
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
    REPORT(single_area_1)
    cur_year <- 1983 ; cur_step <- 1 ; area <- 2
    single_area_2 <- lookup_f
    REPORT(single_area_2)
}, list(lookup_f = single_area_lookup_tad)))
expecteds$single_area_1 <- 198311
expecteds$single_area_2 <- 0

# Single-area lookup form works as expected
single_named_area_lookup_gen <- g3_timeareadata('single_named_area_lookup', read.table(header = TRUE, text = "
year	step	area	total_weight
1983	1	b	198311
1983	2	b	198321
1984	1	b	198411
"), areas = c(a=1,b=2,c=3))

single_named_area_1 <- 0
single_named_area_2 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('single_named_area_lookup')
    cur_year <- 1983 ; cur_step <- 1 ; area <- 1
    single_named_area_1 <- lookup_f
    REPORT(single_named_area_1)
    cur_year <- 1983 ; cur_step <- 2 ; area <- 2
    single_named_area_2 <- lookup_f
    REPORT(single_named_area_2)
}, list(lookup_f = single_named_area_lookup_gen)))
expecteds$single_named_area_1 <- 0
expecteds$single_named_area_2 <- 198321

# no-area form works as expected
no_area_lookup_gen <- g3_timeareadata('no_area_lookup', read.table(header = TRUE, text = "
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
    REPORT(no_area_1)
    cur_year <- 1983 ; cur_step <- 2 ; area <- 2
    no_area_2 <- lookup_f
    REPORT(no_area_2)
}, list(lookup_f = no_area_lookup_gen)))
expecteds$no_area_1 <- 198311
expecteds$no_area_2 <- 198321

# no-step
no_step_lookup_gen <- g3_timeareadata('no_step_lookup', read.table(header = TRUE, text = "
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
    REPORT(no_step_1)
    cur_year <- 1984 ; cur_step <- 2 ; area <- 3
    no_step_2 <- lookup_f
    REPORT(no_step_2)
    cur_year <- 1984 ; cur_step <- 3 ; area <- 2
    no_step_2 <- lookup_f
    REPORT(no_step_3)
}, list(lookup_f = no_step_lookup_gen)))
expecteds$no_step_1 <- 19843
expecteds$no_step_2 <- 19843
expecteds$no_step_3 <- 0

# "Simple" (i.e. mapping to a vector) lookups should return defaults
simple_vec_idx <- 0L
simple_vec_lookup_gen <- gadget3:::g3_intlookup('simple_vec_lookup', c(1, 2, 3), c(2, 3, 4))
simple_vec_1 <- 0
simple_vec_2 <- 0
simple_vec_3 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('simple_vec_lookup')

    simple_vec_idx <- -1L # NB: Out of bounds of what a vector can do in R
    simple_vec_1 <- lookup_f
    REPORT(simple_vec_1)

    simple_vec_idx <- 3L
    simple_vec_2 <- lookup_f
    REPORT(simple_vec_2)

    simple_vec_idx <- 4L
    simple_vec_3 <- lookup_f
    REPORT(simple_vec_3)
}, list(lookup_f = simple_vec_lookup_gen('getdefault', ~simple_vec_idx, -1L))))
expecteds$simple_vec_1 <- -1
expecteds$simple_vec_2 <- 4
expecteds$simple_vec_3 <- -1

# Make sure we can have zero / negative values in a lookup
zero_key_idx <- 0L
zero_key_lookup_gen <- gadget3:::g3_intlookup('zero_key_lookup', c(0, -1, 1), c(2, 3, 4))
zero_key_1 <- 0
zero_key_2 <- 0
zero_key_3 <- 0
actions <- c(actions, gadget3:::f_substitute(~{
    comment('zero_key_lookup')

    zero_key_idx <- 0L
    zero_key_1 <- lookup_f
    REPORT(zero_key_1)

    zero_key_idx <- -1L
    zero_key_2 <- lookup_f
    REPORT(zero_key_2)

    zero_key_idx <- 2L
    zero_key_3 <- lookup_f
    REPORT(zero_key_3)
}, list(lookup_f = zero_key_lookup_gen('getdefault', ~zero_key_idx, -1L))))
expecteds$zero_key_1 <- 2
expecteds$zero_key_2 <- 3
expecteds$zero_key_3 <- -1

###############################################################################

nll <- 0.0
actions <- c(actions, gadget3:::g3l_test_dummy_likelihood(), ~{
    comment('done')
    return(nll)
})

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)
params <- attr(model_cpp, "parameter_template")

# Compare everything we've been told to compare
result <- model_fn(params)
# str(attributes(result), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(attr(result, n), expecteds[[n]]), n)
}
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
