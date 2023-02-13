library(magrittr)
library(unittest)

library(gadget3)

cmp_environment <- function (a, b) {
    ordered_list <- function (x) {
        x <- as.list(x)
        # NB: Can't order an empty list
        if (length(x) > 0) x[order(names(x))] else x
    }

    ut_cmp_identical(ordered_list(a), ordered_list(b))
}

areas <- list(a=1, b=2, c=3, d=4)
stock_a <- g3_stock('stock_a', c(10)) %>% g3s_livesonareas(areas[c('a')])
stock_ac <- g3_stock('stock_ac', c(10)) %>% g3s_livesonareas(unname(areas[c('a', 'c')]))  # NB: Remove names so we generate defaults
stock_bcd <- g3_stock('stock_bcd', c(10)) %>% g3s_livesonareas(areas[c('b', 'c', 'd')])
stock_aggregated <- g3_stock('stock_aggregated', c(10)) %>% g3s_areagroup(list(areas[c('b', 'c')], areas[c('d')]))
stock_1agg <- g3_stock('stock_1agg', c(10)) %>% g3s_areagroup(list(areas[c('b')], areas[c('c')]))

ok(cmp_environment(stock_a$env, list(
    stock__area = 1L,
    stock__area_idx = stock_a$env$stock__area_idx,
    stock__totalareas = 1L,
    stock__areas = as.array(c(a = 1L)),
    area_a = 1L,
    stock__upperlen = Inf,
    stock__minlen = as.array(c("10:Inf" = 10)),
    stock__midlen = structure(as.array(c("10:Inf" = 10.5)), force_vector = TRUE),
    stock__maxlen = as.array(c("10:Inf" = Inf)),
    stock__plusdl = 1,
    stock__dl = 1)), "stock_a: Environment populated with relevant areas")
ok(cmp_environment(stock_ac$env, list(
    stock__totalareas = 2L,
    stock__areas = as.array(c(area1 = 1L, area3 = 3L)),
    area_area1 = 1L,
    area_area3 = 3L,
    stock__upperlen = Inf,
    stock__minlen = as.array(c("10:Inf" = 10)),
    stock__midlen = structure(as.array(c("10:Inf" = 10.5)), force_vector = TRUE),
    stock__maxlen = as.array(c("10:Inf" = Inf)),
    stock__plusdl = 1,
    stock__dl = 1)), "stock_c: Environment populated with default areas")
ok(cmp_environment(stock_bcd$env, list(
    stock__totalareas = 3L,
    stock__areas = as.array(c(b = 2L, c = 3L, d = 4L)),
    area_b = 2L,
    area_c = 3L,
    area_d = 4L,
    stock__upperlen = Inf,
    stock__minlen = as.array(c("10:Inf" = 10)),
    stock__midlen = structure(as.array(c("10:Inf" = 10.5)), force_vector = TRUE),
    stock__maxlen = as.array(c("10:Inf" = Inf)),
    stock__plusdl = 1,
    stock__dl = 1)), "stock_a: Environment populated with relevant areas")
    
cur_time <- 0L  # Initialconditions needs to know what the time is
nll <- 0.0
stock_sum_a_ac <- 0.0
stock_sum_ac_a <- 0.0
stock_sum_ac_bcd <- 0.0
stock_bcd__interacttotals <- g3_stock_instance(stock_bcd, 0)
stock_bcd_a_interactions <- 0L
stock_bcd_ac_interactions <- 0L
actions <- list(
    # NB: livesonareas will add area names to environment, so this works
    g3a_initialconditions(stock_a, ~(if (area == area_a) 1 else if (area == area_b) 2 else if (area == area_c) 3  else if (area == area_d) 4 else 0) * 100 + stock_a__minlen, ~0),
    g3a_initialconditions(stock_ac, ~area * 1000 + stock_ac__minlen, ~0),
    g3a_initialconditions(stock_bcd, ~area * 10000 + stock_bcd__minlen, ~0),
    g3a_initialconditions(stock_aggregated, ~area * 1 + stock_bcd__minlen, ~0),
    g3a_initialconditions(stock_1agg, ~area * 1 + stock_bcd__minlen, ~0),
    list(
        '5' = gadget3:::g3_step(~{
            comment("stock_sum_a_ac")
            stock_iterate(stock_a, stock_intersect(stock_ac, {
                stock_sum_a_ac <- stock_sum_a_ac + sum(stock_ss(stock_a__num)) + sum(stock_ss(stock_ac__num))
            }))
            REPORT(stock_sum_a_ac)

            comment("stock_sum_ac_a")
            stock_iterate(stock_ac, stock_intersect(stock_a, {
                stock_sum_ac_a <- stock_sum_ac_a + sum(stock_ss(stock_ac__num)) + sum(stock_ss(stock_a__num))
            }))
            REPORT(stock_sum_ac_a)

            comment("stock_sum_ac_bcd")
            stock_iterate(stock_ac, stock_intersect(stock_bcd, {
                stock_sum_ac_bcd <- stock_sum_ac_bcd + sum(stock_ss(stock_ac__num)) + sum(stock_ss(stock_bcd__num))
            }))
            REPORT(stock_sum_ac_bcd)

            comment("stock_aggregated stock_a")
            stock_iterate(stock_a, stock_intersect(stock_aggregated, {
                stock_ss(stock_aggregated__num) <-
                    stock_ss(stock_aggregated__num) +
                    stock_ss(stock_a__num)
            }))

            comment("stock_aggregated stock_ac")
            stock_iterate(stock_ac, stock_intersect(stock_aggregated, {
                stock_ss(stock_aggregated__num) <-
                    stock_ss(stock_aggregated__num) +
                    stock_ss(stock_ac__num)
            }))

            comment("stock_aggregated stock_bcd")
            stock_iterate(stock_bcd, stock_intersect(stock_aggregated, {
                stock_ss(stock_aggregated__num) <-
                    stock_ss(stock_aggregated__num) +
                    stock_ss(stock_bcd__num)
            }))

            comment("stock_1agg stock_a")
            stock_iterate(stock_a, stock_intersect(stock_1agg, {
                stock_ss(stock_1agg__num) <-
                    stock_ss(stock_1agg__num) +
                    stock_ss(stock_a__num)
            }))

            comment("stock_1agg stock_ac")
            stock_iterate(stock_ac, stock_intersect(stock_1agg, {
                stock_ss(stock_1agg__num) <-
                    stock_ss(stock_1agg__num) +
                    stock_ss(stock_ac__num)
            }))

            comment("stock_1agg stock_bcd")
            stock_iterate(stock_bcd, stock_intersect(stock_1agg, {
                stock_ss(stock_1agg__num) <-
                    stock_ss(stock_1agg__num) +
                    stock_ss(stock_bcd__num)
            }))

            comment("interact bcd -> a")
            stock_iterate(stock_bcd, stock_interact(stock_a, {
                stock_ss(stock_bcd__interacttotals) <- stock_ss(stock_bcd__interacttotals) + stock_reshape(stock_bcd, stock_ss(stock_a__num))
                stock_bcd_a_interactions <- stock_bcd_a_interactions + area * 1000L + sub_area
            }, prefix = "sub"))
            comment("interact bcd -> ac")
            stock_iterate(stock_bcd, stock_interact(stock_ac, {
                stock_ss(stock_bcd__interacttotals) <- stock_ss(stock_bcd__interacttotals) + stock_reshape(stock_bcd, stock_ss(stock_ac__num))
                stock_bcd_ac_interactions <- stock_bcd_ac_interactions + area * 1000L + sub_area
            }, prefix = "sub"))
        }),
        '999' = ~{
            g3_report_all()
            nll <- nll + g3_param('x', value = 1.0)
            return(nll)
        }))
model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)

params <- attr(model_fn, 'parameter_template')
result <- model_fn(params)
r <- attributes(result)

# We iterated over the stock and populated using the area variable
ok(ut_cmp_identical(
    r$stock_a__num,
    array(
        c(110),
        dim = c(length = 1, area = 1),
        dimnames = list(length = "10:Inf", area = "a"))), "stock_a__num populated, used names from areas lookup")
ok(ut_cmp_identical(
    r$stock_ac__num,
    array(
        c(1010, 3010),
        dim = c(length = 1, area = 2),
        dimnames = list(length = "10:Inf", area = c("area1","area3")))), "stock_ac__num populated, generated default names")
ok(ut_cmp_identical(
    r$stock_bcd__num,
    array(
        c(20010, 30010, 40010),
        dim = c(length = 1, area = 3),
        dimnames = list(length = "10:Inf", area = c("b", "c", "d")))), "stock_bcd__num populated, used names from areas lookup")

ok(ut_cmp_identical(
    r$stock_aggregated__num,
    array(c(
        # NB: Areas b & c --> init + stock_ac + stock_bcd
        (12) + (3010) + (20010 + 30010),
        # NB: Area d --> init + stock_bcd
        14 + 40010), dim = c(length = 1, area = 2), dimnames = list(length = "10:Inf", area = c("area2", "area4")))), "stock_aggregated__num combination of all stocks")

ok(ut_cmp_identical(
    r$stock_1agg__num,
    array(c(
        12 + r$stock_bcd__num[1,'b'],
        13 + r$stock_ac__num[1,'area3'] + r$stock_bcd__num[1,'c']), dim = c(length = 1, area = 2), dimnames = list(length = "10:Inf", area = c("area2.b", "area3.c")))), "stock_1agg__num, got values for areas b & c")

# Intersection works with any combination of single-area stock and multi-area stock
ok(ut_cmp_identical(
    r$stock_sum_a_ac,
    110 + 1010), "stock_sum_a_ac: Only includes area a")
ok(ut_cmp_identical(
    r$stock_sum_ac_a,
    110 + 1010), "stock_sum_ac_a: Only includes area a")
ok(ut_cmp_identical(
    r$stock_sum_ac_bcd,
    3010 + 30010), "stock_sum_ac_bcd: Intersection is area c")

# Interaction is basically intersection for area
ok(ut_cmp_identical(
    r$stock_bcd_a_interactions,
    0L), "stock_bcd_a_interactions: No interaction between bcd & a")
ok(ut_cmp_identical(
    r$stock_bcd_ac_interactions,
    3003L), "stock_bcd_ac_interactions: Interact in area 3 (i.e. c")
ok(ut_cmp_identical(
    r$stock_bcd__interacttotals,
    array(
        c(0, 3010, 0),
        dim = c(length = 1, area = 3),
        dimnames = list(length = "10:Inf", area = c("b", "c", "d")))), "stock_bcd__interacttotals: Summed stock_ac__num in interaction")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
    model_tmb_report <- model_tmb$report()
    for (n in names(attributes(result))) {
        ok(ut_cmp_equal(
            as.vector(model_tmb_report[[n]]),
            as.vector(attr(result, n)),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
} else {
    writeLines("# skip: not running TMB tests")
}
