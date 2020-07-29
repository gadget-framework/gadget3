library(magrittr)
library(unittest)

library(gadget3)

actions <- list()
expecteds <- new.env(parent = emptyenv())

###############################################################################

areas <- g3_areas('a', 'b', 'c')
area <- 1L
cur_step <- 1L
cur_year <- 1980L

tad_lookup <- g3_timeareadata('tad', areas, read.table(header = TRUE, text = "
year	step	area	value
1983	1	a	198311
1983	2	a	198321
1984	1	a	198411
1984	2	a	198421
1983	1	b	198312
1983	2	b	198322
1984	1	b	198412
1984	2	b	198422
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

###############################################################################

actions <- c(actions, ~{
    comment('done')
    return(g3_param('rv'))
})
params <- list(rv=0)

model_fn <- g3_compile_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)

# Compare everything we've been told to compare
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(environment(model_fn)$model_report[[n]], expecteds[[n]]), n)
}

model_cpp <- g3_precompile_tmb(actions, trace = FALSE)
if (!nzchar(Sys.getenv('G3_TEST_TMB'))) { writeLines("# skip: not running TMB tests") ; break }
# model_cpp <- edit(model_cpp)
model_tmb <- g3_tmb_adfun(model_cpp, params)

# Compare everything we've been told to compare
report <- model_tmb$report()
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(report[[n]], expecteds[[n]]), n)
}
