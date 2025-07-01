library(magrittr)
library(unittest)

library(gadget3)

stock_noage <- g3_stock('stock_noage', c(10))
stock_young <- g3_stock('stock_young', c(10)) %>% g3s_age(1, 3)
stock_old <- g3_stock('stock_old', c(10)) %>% g3s_age(4, 6)
stock_inbetween <- g3_stock('stock_inbetween', c(10)) %>% g3s_age(2, 5)
stock_aggregated <- g3_stock('stock_aggregated', c(10)) %>% g3s_agegroup(list(1:2, 5:6))  # NB: Gap in range
stock_inbetween_old_aggregated <- g3_stock('stock_inbetween_old_aggregated', c(10)) %>% g3s_agegroup(list(1:2, 5:6))  # NB: Gap in range

stock_sum_young_inbetween <- 0.0
stock_sum_inbetween_old <- 0.0
stock_sum_young_old <- 0.0
stock_interact_young_old <- 0.0
stock_interact_young_old_vars <- 0.0
nll <- 0.0
actions <- list(
    g3a_time(1999, 1999),
    g3a_initialconditions(stock_noage, ~100 + stock_young__minlen, ~0),
    g3a_initialconditions(stock_young, ~age * 100 + stock_young__minlen, ~0),
    g3a_initialconditions(stock_old, ~age * 1000 + stock_old__minlen, ~0),
    g3a_initialconditions(stock_inbetween, ~age * 10000 + stock_inbetween__minlen, ~0),
    g3a_initialconditions(stock_aggregated, ~age * 1000000 + stock_inbetween__minlen, ~0),
    g3a_initialconditions(stock_inbetween_old_aggregated, ~0 * stock_inbetween__minlen, ~0),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood(),
    list(
        '5:stock_sum_young_noage' = gadget3:::g3_step(g3_formula({
            comment("stock_sum_young_noage")
            stock_iterate(stock_young, stock_intersect(stock_noage, {
                stock_sum_young_noage <- stock_sum_young_noage +
                    sum(stock_ss(stock_young__num)) + sum(stock_ss(stock_noage__num))
            }))
            REPORT(stock_sum_young_noage)
        }, stock_sum_young_noage = 0.0, stock_young = stock_young, stock_noage = stock_noage)),
        '5:stock_sum_noage_old' = gadget3:::g3_step(g3_formula({
            comment("stock_sum_noage_old")
            stock_iterate(stock_noage, stock_intersect(stock_old, {
                stock_sum_noage_old <- stock_sum_noage_old +
                    sum(stock_ss(stock_noage__num)) + sum(stock_ss(stock_old__num))
            }))
            REPORT(stock_sum_noage_old)
        }, stock_sum_noage_old = 0.0, stock_noage = stock_noage, stock_old = stock_old)),
        '5' = gadget3:::g3_step(~{
            comment("stock_sum_young_inbetween")
            stock_iterate(stock_young, stock_intersect(stock_inbetween, {
                stock_sum_young_inbetween <- stock_sum_young_inbetween + sum(stock_ss(stock_young__num)) + sum(stock_ss(stock_inbetween__num))
            }))
            REPORT(stock_sum_young_inbetween)

            comment("stock_sum_inbetween_old")
            stock_iterate(stock_inbetween, stock_intersect(stock_old, {
                stock_sum_inbetween_old <- stock_sum_inbetween_old + sum(stock_ss(stock_inbetween__num)) + sum(stock_ss(stock_old__num))
            }))
            REPORT(stock_sum_inbetween_old)

            comment("stock_sum_young_old")
            stock_iterate(stock_young, stock_intersect(stock_old, {
                stock_sum_young_old <- stock_sum_young_old + sum(stock_ss(stock_young__num)) + sum(stock_ss(stock_old__num))
            }))
            REPORT(stock_sum_young_old)

            comment("stock_interact_young_old")
            stock_iterate(stock_young, stock_interact(stock_old, {
                stock_interact_young_old <- stock_interact_young_old + sum(stock_ss(stock_young__num)) + sum(stock_ss(stock_old__num))
                stock_interact_young_old_vars <- stock_interact_young_old_vars + age * 100000 + sub_age
            }, prefix = 'sub'))
            REPORT(stock_interact_young_old)
            REPORT(stock_interact_young_old_vars)

            comment("stock_inbetween_old_aggregated__num")
            stock_iterate(stock_inbetween, stock_intersect(stock_inbetween_old_aggregated, {
                stock_ss(stock_inbetween_old_aggregated__num) <-
                    stock_ss(stock_inbetween_old_aggregated__num) + stock_ss(stock_inbetween__num)
            }))
            stock_iterate(stock_old, stock_intersect(stock_inbetween_old_aggregated, {
                stock_ss(stock_inbetween_old_aggregated__num) <-
                    stock_ss(stock_inbetween_old_aggregated__num) + stock_ss(stock_old__num)
            }))
            REPORT(stock_inbetween_old_aggregated__num)
        }),
        '999' = ~{
            REPORT(stock_noage__num)
            REPORT(stock_young__num)
            REPORT(stock_old__num)
            REPORT(stock_inbetween__num)
            REPORT(stock_aggregated__num)
            return(nll)
        }))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

params <- attr(model_fn, 'parameter_template')
result <- model_fn(params)
r <- attributes(result)

# We iterated over the stock and populated using the area variable
ok(ut_cmp_identical(
    r$stock_young__num,
    array(
        c(110, 210, 310),
        dim = c(length = 1, age = 3),
        dimnames = list(length = "10:Inf", age = c("age1","age2","age3")))), "stock_young__num populated")
ok(ut_cmp_identical(
    r$stock_old__num,
    array(
        c(4010, 5010, 6010),
        dim = c(length = 1, age = 3),
        dimnames = list(length = "10:Inf", age = c("age4","age5","age6")))), "stock_old__num populated")
ok(ut_cmp_identical(
    r$stock_inbetween__num,
    array(
        c(20010, 30010, 40010, 50010),
        dim = c(length = 1, age = 4),
        dimnames = list(length = "10:Inf", age = c("age2","age3","age4","age5")))), "stock_inbetween__num populated")
ok(ut_cmp_identical(
    r$stock_aggregated__num,
    array(
        c(1000010, 5000010),
        dim = c(length = 1, age = 2),
        dimnames = list(length = "10:Inf", age = c("1:2","5:6")))), "stock_aggregated__num populated")

ok(ut_cmp_identical(
    r$stock_inbetween_old_aggregated__num,
    array(
        c(20010, 50010 + 5010 + 6010),
        dim = c(length = 1, age = 2),
        dimnames = list(length = "10:Inf", age = c("1:2","5:6")))), "stock_inbetween_old_aggregated__num populated")

# Intersection between stocks without age means we iterate over
ok(ut_cmp_identical(
    r$stock_sum_noage_old,
    sum(r$stock_old__num) + as.vector(r$stock_noage__num) * 3 ), "stock_sum_noage_old: summed noage for each of the old ages")
ok(ut_cmp_identical(
    r$stock_sum_young_noage,
    as.vector(r$stock_noage__num) * 3 + sum(r$stock_young__num) ), "stock_sum_young_noage: summed noage for each of the young ages")

# Intersection works with any combination of single-area stock and multi-area stock
ok(ut_cmp_identical(
    r$stock_sum_young_inbetween,
    210 + 310 + 20010 + 30010), "stock_sum_young_inbetween: Ages 2+3")
ok(ut_cmp_identical(
    r$stock_sum_inbetween_old,
    4010 + 5010 + 40010 + 50010), "stock_sum_inbetween_old: Ages 4+5")
ok(ut_cmp_identical(
    r$stock_sum_young_old,
    0), "stock_sum_young_old: No intersection")

# Interact results in a combinatorial explosion
ok(ut_cmp_identical(
    r$stock_interact_young_old,
    sum(expand.grid(r$stock_young__num, r$stock_old__num))), "stock_interact_young_old: Combinatorial explosion of 2 stocks")
ok(ut_cmp_identical(
    r$stock_interact_young_old_vars,
    sum(1:3) * length(4:6) * 100000 + sum(4:6) * length(1:3)), "stock_interact_young_old_vars: Sum of each age combination")


gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("g3s_age is idempotent", {
    ok(ut_cmp_identical(
        g3_stock_instance(g3_stock("x", c(1,10)) %>% g3s_age(1, 10) %>% g3s_livesonareas(1)),
        g3_stock_instance(g3_stock("x", c(1,10)) %>% g3s_age(2, 5) %>% g3s_livesonareas(1) %>% g3s_age(1, 10))), "Replacing age results in identical instance")
})
