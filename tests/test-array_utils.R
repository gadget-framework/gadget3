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
ok(ut_cmp_identical(g3_array_agg(ar, "length"), structure(
    c(`50:60` = sum(ar[length = 1,,]), `60:70` = sum(ar[length = 2,,]), `70:Inf` = sum(ar[length = 3,,])),
    dim = c(length = 3L),
    dimnames = list(length = c("50:60", "60:70", "70:Inf")))), "Single aggregate is still an array, with naming")

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

ok_group("lists of arrays") ###################################################

ar1 <- gen_arr(
    age = 5:15,
    time = list(2000:2004, 1:2) )
ar2 <- gen_arr(
    age = 10:20,
    time = list(2000:2004, 1:2) )
ok(ut_cmp_identical(list(x = ar1, y = ar2) |> g3_array_agg("step"), list(
    x = structure(
            c(`1` = sum(ar1[,grepl("-01$", dimnames(ar1)$time)]), `2` = sum(ar1[,grepl("-02$", dimnames(ar1)$time)])),
            dim = c(step = 2L),
            dimnames = list(step = c("1", "2"))),
    y = structure(
            c(`1` = sum(ar2[,grepl("-01$", dimnames(ar2)$time)]), `2` = sum(ar2[,grepl("-02$", dimnames(ar2)$time)])),
            dim = c(step = 2L),
            dimnames = list(step = c("1", "2")))) ), "Can process a list of arrays, names kept")

ok_group("g3_array_combine") ##################################################

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
ok(ut_cmp_equal(
    g3_array_combine(list(
        c("2000" = 20, "2001" = 21, "2002" = 22),
        c("2001" = 100, "2002" = 200, "2003" = 300) )),
    c("2000" = 20, "2001" = 121, "2002" = 222, "2003" = 300)) ,"g3_array_combine: Can combine vectors too")

ok(ut_cmp_equal(
    list(ar1, ar2) |> g3_array_agg("year") |> g3_array_combine(),
    g3_array_agg(ar1, "year") + g3_array_agg(ar2, "year") ), "g3_array_combine: Can agg then combine a list of arrays")

wtm <- function(ar, meas, ...) {
    x <- g3_array_agg(ar, meas, ..., opt_length_midlen = TRUE)
    x <- rep(as.numeric(names(x)), x)  # Unfold into a list of counts
    mean(x)
}
wtsd <- function(ar, meas, ...) {
    x <- g3_array_agg(ar, meas, ..., opt_length_midlen = TRUE)
    x <- rep(as.numeric(names(x)), x)  # Unfold into a list of counts
    sd(x)
}
ar <- gen_arr(
    length = c(50, 60, 70),
    age = 1:10,
    area = 1:3 )
for (i in 1:10) ok(ut_cmp_equal(
    as.vector(g3_array_agg(ar, c("age"), agg = "length_mean")[i]),
    as.vector(wtm(ar, "length", age = i))), paste0("length_mean: age ", i))
for (i in 1:10) for (j in 1:3) ok(ut_cmp_equal(
    as.vector(g3_array_agg(ar, c("age", "area"), agg = "length_mean")[age = i, area = j]),
    as.vector(wtm(ar, "length", age = i, area = j))), paste0("length_mean: age ", i, ", area ", j))
for (i in 1:10) ok(ut_cmp_equal(
    as.vector(g3_array_agg(ar, c("age"), agg = "length_sd")[i]),
    as.vector(wtsd(ar, "length", age = i))), paste0("length_sd: age ", i))
for (i in 1:10) for (j in 1:3) ok(ut_cmp_equal(
    as.vector(g3_array_agg(ar, c("age", "area"), agg = "length_sd")[age = i, area = j]),
    as.vector(wtsd(ar, "length", age = i, area = j))), paste0("length_sd: age ", i, ", area ", j))

ar <- gen_arr(
    length = c(50, 60, 70),
    predator_length = c(100, 200),
    area = 1:3 )
for (i in 1:3) ok(ut_cmp_equal(
    as.vector( g3_array_agg(ar, c("area"), agg ="predator_length_mean")[i] ),
    as.vector( wtm(ar, "predator_length", area = i) )), paste0("predator_length_mean: area ", i))
