if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

# ut_cmp_equal, but strip off dimensions first
ut_cmp_vec <- function(a, b, ...) ut_cmp_equal(as.vector(a), as.vector(b), ...)

# Generate random array from dimnames input
gen_arr <- function(...) {
    dn <- list(...)

    if ("time" %in% names(dn)) {
        dn$time <- paste(
            rep(dn$time[[1]], each = length(dn$time[[2]])),
            sprintf("%02d", dn$time[[2]]),
            sep = "-")
    }
    if ("age" %in% names(dn)) {
        dn$age <- paste0("age", dn$age)
    }
    if ("length" %in% names(dn)) {
        dn$length <- paste(
            dn$length,
            c(tail(dn$length, -1), "Inf"),
            sep = ":" )
    }
    if ("predator_length" %in% names(dn)) {
        dn$predator_length <- paste(
            dn$predator_length,
            c(tail(dn$predator_length, -1), "Inf"),
            sep = ":" )
    }

    d <- vapply(dn, length, integer(1))
    array(
        floor(runif(prod(d), 1e5, 1e6)),
        dim = d,
        dimnames = dn)
}

ok_group("time_split") ########################################################

ar <- gen_arr(
    length = c(50, 60, 70),
    age = 0:10,
    time = list(2000:2004, 1:2) )
ok(ut_cmp_vec(
    g3_array_agg(ar, opt_time_split = TRUE)[,,step = "1", year = "2002"],
    ar[,,time = "2002-01"],
    end = NULL ), "opt_time_split returns same values [2002-01]")
ok(ut_cmp_vec(
    g3_array_agg(ar, opt_time_split = TRUE)[,,step = "2", year = "2004"],
    ar[,,time = "2004-02"],
    end = NULL ), "opt_time_split returns same values [2004-02]")

ar <- gen_arr(
    time = list(2000:2004, 1:4),
    age = 0:10 )
ok(ut_cmp_equal(
    g3_array_agg(ar, opt_time_split = TRUE)[step = "3", year = "2001",],
    ar[time = "2001-03",],
    end = NULL ), "opt_time_split returns same values, time at start [2001-03]")

ar <- gen_arr(
    time = list(2000:2004, 1:4),
    age = 0:10 )
ok(ut_cmp_equal(
    g3_array_agg(ar, opt_time_split = FALSE),
    ar,
    end = NULL ), "Can turn opt_time_split off")
ok(ut_cmp_equal(
    names(g3_array_agg(ar, c("year"))),
    as.character(2000:2004),
    end = NULL ), "Aggregated by year when asked")
ok(ut_cmp_equal(
    names(g3_array_agg(ar, c("time"))),
    paste0(rep(2000:2004, each = 4), c("-01", "-02", "-03", "-04")),
    end = NULL ), "...or by time")

ar <- gen_arr(
    length = c(50, 60, 70),
    age = 0:10,
    time = list(2000:2004, 1:2) )
ok(ut_cmp_vec(
    g3_array_agg(ar, c("length"), year = 2004),
    apply(ar[,,time = c("2004-01", "2004-02")], 'length', sum),
    end = NULL ), "time_split turns on when filtering by year")
ok(ut_cmp_vec(
    g3_array_agg(ar, c("length"), time = c("2001-01", "2002-02")),
    apply(ar[,,time = c("2001-01", "2002-02")], 'length', sum),
    end = NULL ), "time_split turns off when filtering by time")

ok_group("filtering") #########################################################

ar <- gen_arr(
    length = c(50, 60, 70),
    age = 0:10,
    time = list(2000:2004, 1:2) )
ok(ut_cmp_vec(
    # TODO: Having to set margins manually, since otherwise we assume time
    g3_array_agg(ar, age = 4, year = 2001:2002, step = 2),
    ar[,age = "age4",time = paste0(2001:2002, "-02")],
    end = NULL ), "Can use numeric age/year/step, get converted")

ok_group("grouping") ##########################################################

ar <- gen_arr(
    length = c(50, 60, 70),
    age = 0:10,
    time = list(2000:2004, 1:2) )
ok(ut_cmp_vec(
    g3_array_agg(ar, margins = c("year", "length"), year = 2002:2003),
    c(
        apply(ar[,,time = c("2002-01", "2002-02")], "length", sum),
        apply(ar[,,time = c("2003-01", "2003-02")], "length", sum),
        NULL ),
    end = NULL ), "Can filter/aggregate year at the same time")

ok_group("length") ############################################################

ar <- gen_arr(
    length = c(50, 60, 70),
    age = 0:10 )
ok(ut_cmp_identical(
    dimnames(g3_array_agg(ar, opt_length_midlen = TRUE)),
    list(
        length = c("55", "65", "75"),
        age = paste0("age", 0:10) )), "Turning on opt_length_midlen converted dimnames to midlength")

ar <- gen_arr(
    length = seq(10, 100, 10) )
ok(ut_cmp_vec(
    g3_array_agg(ar, length = c(50, 75, 200)),
    ar[length = c("50:60", "70:80", "100:Inf"), drop = F],
    end = NULL), "Can select lengthgroups by using any value within the grouping")

ar <- gen_arr(
    length = c(50, 60, 70),
    age = 0:10 )
ok(ut_cmp_vec(
    g3_array_agg(ar, length = 65, opt_length_midlen = TRUE),
    ar[length = "60:70",],
    end = NULL), "opt_length_midlen doesn't prevent being able to select by single integers")

ar <- gen_arr( length = c(0) )
ok(ut_cmp_identical(
    dimnames(g3_array_agg(ar, opt_length_midlen = TRUE)),
    list(
        length = NA_character_ )), "opt_length_midlen turns 0:Inf to NA")

ar <- gen_arr( predator_length = c(0) )
ok(ut_cmp_identical(
    dimnames(g3_array_agg(ar, opt_length_midlen = TRUE)),
    list(
        predator_length = NA_character_ )), "opt_length_midlen turns predator_length 0:Inf to NA")

ar1 <- gen_arr(
    age = 5:15,
    time = list(2000:2004, 1:2) )
ar2 <- gen_arr(
    age = 10:20,
    time = list(2000:2004, 1:2) )
ok(ut_cmp_equal(
    g3_array_combine(list(ar1, ar2)),
    g3_array_combine(list(ar2, ar1)) ), "g3_array_combine: Order irrelevant")
for (t in seq_along(dimnames(ar1)$time)) ok(ut_cmp_equal(g3_array_combine(list(ar1, ar2))[,t], c(
    age5 = ar1["age5", t] + 0,
    age6 = ar1["age6", t] + 0,
    age7 = ar1["age7", t] + 0,
    age8 = ar1["age8", t] + 0,
    age9 = ar1["age9", t] + 0,
    age10 = ar1["age10", t] + ar2["age10", t],
    age11 = ar1["age11", t] + ar2["age11", t],
    age12 = ar1["age12", t] + ar2["age12", t],
    age13 = ar1["age13", t] + ar2["age13", t],
    age14 = ar1["age14", t] + ar2["age14", t],
    age15 = ar1["age15", t] + ar2["age15", t],
    age16 = 0 + ar2["age16", t],
    age17 = 0 + ar2["age17", t],
    age18 = 0 + ar2["age18", t],
    age19 = 0 + ar2["age19", t],
    age20 = 0 + ar2["age20", t] )), paste0("g3_array_combine: time ", t, " combined as expected"))
