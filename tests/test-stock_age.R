library(magrittr)
library(unittest)

library(gadget3)

stock_young <- g3_stock('stock_young', c(10)) %>% g3s_age(1, 3)
stock_old <- g3_stock('stock_old', c(10)) %>% g3s_age(4, 6)
stock_inbetween <- g3_stock('stock_inbetween', c(10)) %>% g3s_age(2, 5)
stock_aggregated <- g3_stock('stock_aggregated', c(10)) %>% g3s_agegroup(list(1:2, 5:6))  # NB: Gap in range
stock_inbetween_old_aggregated <- g3_stock('stock_inbetween_old_aggregated', c(10)) %>% g3s_agegroup(list(1:2, 5:6))  # NB: Gap in range

cur_time <- 0L  # Initialconditions needs to know what the time is
stock_sum_young_inbetween <- 0.0
stock_sum_inbetween_old <- 0.0
stock_sum_young_old <- 0.0
stock_interact_young_old <- 0.0
stock_interact_young_old_vars <- 0.0
nll <- 0.0
actions <- list(
    g3a_initialconditions(stock_young, ~age * 100 + stock_young__minlen, ~0),
    g3a_initialconditions(stock_old, ~age * 1000 + stock_old__minlen, ~0),
    g3a_initialconditions(stock_inbetween, ~age * 10000 + stock_inbetween__minlen, ~0),
    g3a_initialconditions(stock_aggregated, ~age * 1000000 + stock_inbetween__minlen, ~0),
    g3a_initialconditions(stock_inbetween_old_aggregated, ~0 * stock_inbetween__minlen, ~0),
    list(
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
            REPORT(stock_young__num)
            REPORT(stock_old__num)
            REPORT(stock_inbetween__num)
            REPORT(stock_aggregated__num)
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


if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
    model_tmb_report <- model_tmb$report()

    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
} else {
    writeLines("# skip: not running TMB tests")
}

ok_group("g3s_age is idempotent", {
    ok(ut_cmp_identical(
        g3_stock_instance(g3_stock("x", c(1,10)) %>% g3s_age(1, 10) %>% g3s_livesonareas(1)),
        g3_stock_instance(g3_stock("x", c(1,10)) %>% g3s_age(2, 5) %>% g3s_livesonareas(1) %>% g3s_age(1, 10))), "Replacing age results in identical instance")
})
