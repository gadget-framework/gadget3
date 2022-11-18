library(magrittr)
library(unittest)

library(gadget3)

cmp_code <- function (a, b) ut_cmp_identical(deparse(a), deparse(b))

ok_group("step_id", {
    step_id <- gadget3:::step_id
    stock_a <- g3_stock('stock_aaa', seq(10, 35, 5))
    stock_b <- g3_stock('stock_bbb', seq(10, 35, 5))

    ok(ut_cmp_identical(step_id("camel"), "camel               "), "String: camel")
    ok(ut_cmp_error({
        step_id(list(4))
    }, "list\\(4\\)"), "General lists not allowed")

    ok(ut_cmp_identical(step_id(0), "000"), "Padding numbers: 0")
    ok(ut_cmp_identical(step_id(99), "099"), "Padding numbers: 99")
    ok(ut_cmp_identical(step_id(100), "100"), "Padding numbers: 100")

    ok(ut_cmp_identical(step_id(stock_a), "stock_aaa           "), "Stock name: stock_a")
    ok(ut_cmp_identical(step_id(stock_b), "stock_bbb           "), "Stock name: stock_b")

    ok(ut_cmp_identical(step_id(0, 10, 200), "000:010:200"), "Multiple numbers: 0, 10, 200")
    ok(ut_cmp_identical(step_id(0, stock_a, "camel"), "000:stock_aaa           :camel               "), "All the things")
})

ok_group("g3_step:stock_reshape", {
    source <- g3_stock('source', seq(10, 40, 10))
    source__num <- g3_stock_instance(source)
    source__wgt <- g3_stock_instance(source)
    dest_even <- g3_stock('dest_even', seq(10, 40, 10))  # Input == output
    dest_even__num <- g3_stock_instance(dest_even)
    dest_even__wgt <- g3_stock_instance(dest_even)
    dest_combine <- g3_stock('dest_combine', seq(10, 40, 40))  # All input combined
    dest_combine__num <- g3_stock_instance(dest_combine)
    dest_combine__wgt <- g3_stock_instance(dest_combine)
    dest_2group <- g3_stock('dest_2group', seq(10, 40, 20))  # 2 groups
    dest_2group__num <- g3_stock_instance(dest_2group)
    dest_2group__wgt <- g3_stock_instance(dest_2group)
    dest_wider <- g3_stock('dest_wider', seq(0, 90, 10))  # Wider top and bottom
    dest_wider__num <- g3_stock_instance(dest_wider)
    dest_wider__wgt <- g3_stock_instance(dest_wider)
    dest_nolength <- gadget3:::g3_storage('dest_nolength')  # No length at all
    dest_nolength__num <- g3_stock_instance(dest_nolength)

    cur_time <- 0L  # Initialconditions needs to know what the time is
    nll <- 0.0
    actions <- list(
        g3a_initialconditions(source, ~g3_param_vector("source_num"), ~g3_param_vector("source_wgt")),

        list('900:dest_even' = gadget3:::g3_step(~stock_iterate(dest_even, stock_intersect(source, {
            stock_ss(dest_even__num) <- stock_reshape(dest_even, stock_ss(source__num))
            REPORT(dest_even__num)
        })))),

        list('900:dest_combine' = gadget3:::g3_step(~stock_iterate(dest_combine, stock_intersect(source, {
            stock_ss(dest_combine__num) <- stock_reshape(dest_combine, stock_ss(source__num))
            REPORT(dest_combine__num)
        })))),

        list('900:dest_2group' = gadget3:::g3_step(~stock_iterate(dest_2group, stock_intersect(source, {
            stock_ss(dest_2group__num) <- stock_reshape(dest_2group, stock_ss(source__num))
            REPORT(dest_2group__num)
        })))),

        list('900:dest_wider' = gadget3:::g3_step(~stock_iterate(dest_wider, stock_intersect(source, {
            stock_ss(dest_wider__num) <- stock_reshape(dest_wider, stock_ss(source__num))
            REPORT(dest_wider__num)
        })))),

        list('900:dest_nolength' = gadget3:::g3_step(~stock_iterate(dest_nolength, stock_intersect(source, {
            stock_ss(dest_nolength__num) <- stock_reshape(dest_nolength, stock_ss(source__num))
            REPORT(dest_nolength__num)
        })))),

        list('999' = ~{
            nll <- nll + g3_param('x')
            return(nll)
        }))

    # Compile model
    params <- list(
        source_num = c(11, 22, 33, 44),
        source_wgt = c(11, 22, 33, 44),
        x = 1.0)
    model_fn <- g3_to_r(actions)
    # model_fn <- edit(model_fn)
    result <- model_fn(params)

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions, trace = FALSE)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
    } else {
        writeLines("# skip: not compiling TMB model")
        model_cpp <- c()
    }

    ok(ut_cmp_identical(
        sort(all.vars(body(model_fn))[endsWith(all.vars(body(model_fn)), '_lgmatrix')]),
        c("source_dest_2group_lgmatrix",
            "source_dest_combine_lgmatrix",
            "source_dest_wider_lgmatrix")), "Generated matrices for mismatched stocks, not dest_even")

    # str(attributes(result))
    ok(ut_cmp_equal(
        as.vector(attr(result, 'dest_even__num')),
        c(11, 22, 33, 44)), "dest_even__num")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'dest_combine__num')),
        c(11 + 22 + 33 + 44)), "dest_combine__num")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'dest_2group__num')),
        c(11 + 22, 33 + 44)), "dest_2group__num")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'dest_wider__num')),
        c(0, 11, 22, 33, 44, 0, 0, 0, 0, 0)), "dest_wider__num")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'dest_nolength__num')),
        sum(11, 22, 33, 44)), "dest_nolength__num")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group("g3_step:stock_ss", {
     stock <- g3_stock('halibut', 1:10) %>% g3s_age(1,10) %>% g3s_livesonareas(1)
     stock__num <- g3_stock_instance(stock)
     ok(cmp_code(
         gadget3:::g3_step(~stock_ss(stock__num, area = 5)),
         ~stock__num[, stock__age_idx, 5]), "Can replace individual dimension subsets with something else (area)")
     ok(cmp_code(
         gadget3:::g3_step(~stock_ss(stock__num, age = i + 1)),
         ~stock__num[, i + 1, stock__area_idx]), "Can replace individual dimension subsets with something else (age)")
     ok(cmp_code(
         gadget3:::g3_step(~stock_ss(stock__num, area = , age = j)),
         ~stock__num[, j, ]), "Missing values are honoured too")

     ok(cmp_code(
         gadget3:::g3_step(~stock_ss(stock__num, length = 0L)),
         ~stock__num[0L, stock__age_idx, stock__area_idx]), "Length can be overriden with a constant")
     ok(cmp_code(
         gadget3:::g3_step(~stock_ss(stock__num, length = default)),
         ~stock__num[stock__length_idx, stock__age_idx, stock__area_idx]), "Length can be turned back on again")
})

ok_group("g3_step:stock_switch", {
    # NB: Differing names, ordinarily stock_imm would be "prey_stock", e.g.
    stock_imm <- g3_stock('ling_imm', c(1))
    stock_mat <- g3_stock('ling_mat', c(1))
    stock_zat <- g3_stock('ling_zat', c(1))

    ok(cmp_code(
        gadget3:::g3_step(~stock_switch(stock_imm, ling_imm = 123, ling_mat = 456, ling_pat = 789)),
        ~123), "stock_imm: Chose ling_imm value")
    ok(cmp_code(
        gadget3:::g3_step(~stock_switch(stock_mat, ling_imm = 123, ling_mat = 456, ling_pat = 789)),
        ~456), "stock_mat: Chose ling_mat value")
    ok(ut_cmp_error(
        gadget3:::g3_step(~stock_switch(stock_zat, ling_imm = 123, ling_mat = 456, ling_pat = 789)),
        "ling_zat"), "stock_zat: No default, threw an error")
    ok(cmp_code(
        gadget3:::g3_step(~stock_switch(stock_zat, ling_imm = 123, ling_mat = 456, ling_pat = 789, 999)),
        ~999), "stock_zat: Chose default value")
})

ok_group("g3_step:dependent_formulas", (function () {
    stock_imm <- g3s_age(g3_stock('ling_imm', 1), 1, 3)
    stock_imm__num <- g3_stock_instance(stock_imm, 0)
    stock_area <- g3s_livesonareas(g3s_age(g3_stock('area_imm', 1), 1, 3), 1:2)
    stock_area__num <- g3_stock_instance(stock_area, 0)

    by_age_f <- ~ 2 * age
    f <- gadget3:::g3_step(~stock_iterate(stock_imm, stock_ss(stock_imm__num) + by_age_f))
    ok(cmp_code(f, ~for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) g3_with(
        ling_imm__age_idx := g3_idx(age - ling_imm__minage + 1L),
        by_age_f := (2 * age),
        (ling_imm__num[, ling_imm__age_idx] + by_age_f))), "by_age_f: 2 * age gets inserted inside loop")

    f <- gadget3:::g3_step(~stock_iterate(stock_area, stock_ss(stock_area__num) + by_age_f))
    ok(cmp_code(f, ~for (area_imm__area_idx in seq_along(area_imm__areas)) g3_with(
        area := area_imm__areas[[area_imm__area_idx]],
        for (age in seq(area_imm__minage, area_imm__maxage, by = 1)) g3_with(
            area_imm__age_idx := g3_idx(age - area_imm__minage + 1L),
            by_age_f := (2 * age),
            (area_imm__num[, area_imm__age_idx, area_imm__area_idx] + by_age_f)))), "by_age_f: 2 * age gets inserted inside double loop")

    independent_f <- ~2 * stock_imm__minage
    f <- gadget3:::g3_step(~stock_iterate(stock_imm, stock_ss(stock_imm__num) + independent_f))
    ok(cmp_code(f, ~g3_with(independent_f := (2 * ling_imm__minage), for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) g3_with(
        ling_imm__age_idx := g3_idx(age - ling_imm__minage + 1L),
        (ling_imm__num[, ling_imm__age_idx] + independent_f)))), "independent_f: 2 gets inserted outside loop, still renamed though")

    independent_switch_f <- ~2 * stock_switch(stock_imm, ling_imm = 22 + 33)
    f <- gadget3:::g3_step(~stock_iterate(stock_imm, stock_ss(stock_imm__num) + independent_switch_f))
    ok(cmp_code(f, ~g3_with(independent_switch_f := (2 * (22 + 33)), for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) g3_with(
        ling_imm__age_idx := g3_idx(age - ling_imm__minage + 1L),
        (ling_imm__num[, ling_imm__age_idx] + independent_switch_f)))), "independent_switch_f: stock_switch() resolved")

    independent_f <- ~2 * stock_area__minage
    f <- gadget3:::g3_step(~stock_iterate(stock_area, stock_ss(stock_area__num) + independent_f))
    ok(cmp_code(f, ~g3_with(independent_f := (2 * area_imm__minage), for (area_imm__area_idx in seq_along(area_imm__areas)) g3_with(
        area := area_imm__areas[[area_imm__area_idx]],
        for (age in seq(area_imm__minage, area_imm__maxage, by = 1)) g3_with(
            area_imm__age_idx := g3_idx(age - area_imm__minage + 1L),
            (area_imm__num[, area_imm__age_idx, area_imm__area_idx] + independent_f))))), "independent_f: 2 gets inserted outside double loop, still renamed though")

    global_f <- gadget3:::g3_global_formula(~4 * age, init_val = ~4 + 4)
    f <- gadget3:::g3_step(~stock_iterate(stock_imm, stock_ss(stock_imm__num) + global_f))
    ok(cmp_code(f, ~for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) g3_with(
        ling_imm__age_idx := g3_idx(age - ling_imm__minage + 1L),
        {
            global_f <- 4 * age
            (ling_imm__num[, ling_imm__age_idx] + global_f)
        })), "global_f: iterative case gets inserted inside loop")
    ok(any(grepl("global_f <- 4 + 4", deparse(g3_to_r(list(f))), fixed = TRUE)), "global_f: init_val in header when fully compiled")

    global_ind_f <- gadget3:::g3_global_formula(~4 * 99, init_val = ~4 + 6)
    f <- gadget3:::g3_step(~stock_iterate(stock_imm, stock_ss(stock_imm__num) + global_ind_f))
    ok(cmp_code(f, ~{
        global_ind_f <- 4 * 99
        for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) g3_with(
            ling_imm__age_idx := g3_idx(age - ling_imm__minage + 1L),
            (ling_imm__num[, ling_imm__age_idx] + global_ind_f))
    }), "global_ind_f: iterative case gets inserted outside loop")
    ok(any(grepl("global_ind_f <- 4 + 6", deparse(g3_to_r(list(f))), fixed = TRUE)), "global_ind_f: init_val in header when fully compiled")

    global_init_f <- gadget3:::g3_global_formula(init_val = ~2 * 2)
    f <- gadget3:::g3_step(~stock_iterate(stock_imm, stock_ss(stock_imm__num) + global_init_f))
    ok(cmp_code(f, ~for (age in seq(ling_imm__minage, ling_imm__maxage, by = 1)) g3_with(
        ling_imm__age_idx := g3_idx(age - ling_imm__minage + 1L),
        (ling_imm__num[, ling_imm__age_idx] + global_init_f))), "global_ind_f: global_ind_f not mentioned anywhere in formula")
    ok(any(grepl("global_init_f <- 2 * 2", deparse(g3_to_r(list(f))), fixed = TRUE)), "global_init_f: init_val in header when fully compiled")
})())

ok_group("g3_step:stock_param", {
    stock_a <- g3_stock(c(t = 'stock', q = 'stick', 'aaa'), seq(10, 35, 5))

    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param(stock_a, 'parr', optmise = FALSE)
            stock_param(stock_a, 'parr', optmise = FALSE, upper = 5)
        }), ~{
            g3_param("stock_stick_aaa.parr", optmise = FALSE)
            g3_param("stock_stick_aaa.parr", optmise = FALSE, upper = 5)
        }), "Passed through options")
    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param(stock_a, name_part = 't', 'parr')
            stock_param(stock_a, 'parr', name_part = 't')
            stock_param(stock_a, 'parr', name_part = 't', lower = 4, upper = 5)
        }), ~{
            g3_param("stock.parr")
            g3_param("stock.parr")
            g3_param("stock.parr", lower = 4, upper = 5)
        }), "name_part can be either beffore or after name, not passed through to g3_param call")
    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param(stock_a, name_part = c('t', 'q'), 'par1')
            stock_param(stock_a, name_part = c('q', 't'), 'par1')
            stock_param(stock_a, name_part = c('t'), 'par1')
        }), ~{
            g3_param("stock_stick.par1")
            g3_param("stick_stock.par1")
            g3_param("stock.par1")
        }), "name_part can contain multiple name_parts, get used in order")
})

ok_group("g3_step:stock_param_table", {
    stock_a <- g3_stock(c(t = 'stock', 'aaa'), seq(10, 35, 5))
    stock_b <- g3_stock(c(t = 'stock', 'bbb'), seq(10, 35, 5))

    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param_table(stock_a, 'par1', data.frame(age = seq(stock_a__minage, stock_a__maxage), year = 2:3))
            stock_param_table(stock_a, 'par1', data.frame(age = seq(stock_a__minage, stock_a__maxage), len = stock_a__minlen))
        }), ~{
            g3_param_table("stock_aaa.par1", data.frame(age = seq(stock_aaa__minage, stock_aaa__maxage), year = 2:3))
            g3_param_table("stock_aaa.par1", data.frame(age = seq(stock_aaa__minage, stock_aaa__maxage), len = stock_aaa__minlen))
        }), "renamed parts in table_defn")

    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param_table(stock_a, 'par1', data.frame(year = 2:3), upper = 5)
            stock_param_table(stock_a, 'par1', data.frame(year = 2:3), upper = 5, lower = 2)
        }), ~{
            g3_param_table('stock_aaa.par1', data.frame(year = 2:3), upper = 5)
            g3_param_table('stock_aaa.par1', data.frame(year = 2:3), upper = 5, lower = 2)
        }), "passed through remaining params")

    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param_table(stock_a, name_part = 't', 'par1', data.frame(year = 2:3), upper = 5)
            stock_param_table(stock_a, 'par1', name_part = 't', data.frame(year = 2:3), upper = 5, lower = 2)
        }), ~{
            g3_param_table('stock.par1', data.frame(year = 2:3), upper = 5)
            g3_param_table('stock.par1', data.frame(year = 2:3), upper = 5, lower = 2)
        }), "Can use name_part before or after actual name")

    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param_table(stock_a, name_part = 't', 'par1', stock_with(stock_b, data.frame(age = seq(stock_a__minage, stock_b__maxage))))
            stock_param_table(stock_b, name_part = 't', 'par1', stock_with(stock_a, data.frame(age = seq(stock_a__minage, stock_b__maxage))))
        }), ~{
            g3_param_table('stock.par1', data.frame(age = seq(stock_aaa__minage, stock_bbb__maxage)))
            g3_param_table('stock.par1', data.frame(age = seq(stock_aaa__minage, stock_bbb__maxage)))
        }), "Can stock_with other stocks into table_defn")

    stock_a <- g3_stock(c(t = 'stock', q = 'stick', 'aaa'), seq(10, 35, 5))
    ok(cmp_code(
        gadget3:::g3_step(~{
            stock_param_table(stock_a, name_part = c('t', 'q'), 'par1', data.frame(year = 2:3))
            stock_param_table(stock_a, name_part = c('q', 't'), 'par1', data.frame(year = 2:3))
            stock_param_table(stock_a, name_part = c('t'), 'par1', data.frame(year = 2:3))
        }), ~{
            g3_param_table("stock_stick.par1", data.frame(year = 2:3))
            g3_param_table("stick_stock.par1", data.frame(year = 2:3))
            g3_param_table("stock.par1", data.frame(year = 2:3))
        }), "name_part can contain multiple name_parts, get used in order")
})

ok_group("list_to_stock_switch", {
    # NB: Differing names, ordinarily stock_imm would be "prey_stock", e.g.
    stock_imm <- g3_stock('ling_imm', c(1))
    stock_mat <- g3_stock('ling_mat', c(1))
    stock_zat <- g3_stock('ling_zat', c(1))
    do_ss <- function (stock, l) {
        f <- gadget3:::list_to_stock_switch(l)
        assign('stock', stock, envir = environment(f))
        gadget3:::g3_step(f)
    }

    ok(ut_cmp_error(
        gadget3:::list_to_stock_switch(list(1,2)),
        "one default"), "Only one default option allowed")
    ok(ut_cmp_error(
        gadget3:::list_to_stock_switch(list(a = 1, 2, 3)),
        "one default"), "Only one default option allowed")

    out <- do_ss(stock_zat, list(99))
    ok(cmp_code(out, ~99), "Single default item")
    out <- do_ss(stock_zat, list(ling_imm = quote(2 + 2), ling_zat = 343, 99))
    ok(cmp_code(out, ~343), "Mixed value types")

    out <- do_ss(stock_zat, list(ling_imm = quote(2 + 2), ling_zat = g3_formula(stock__midlen^x, x = 2), 99))
    ok(cmp_code(out, ~(ling_zat__midlen^x)), "Formula as output value, stock substitutions happened")
    ok(ut_cmp_equal(environment(out)$x, 2), "Formula innards got copied")

    out <- do_ss(stock_imm, g3_formula(stock__midlen^x, x = 2))
    ok(cmp_code(out, ~(ling_imm__midlen^x)), "Bare formula is treated as default")
    out <- do_ss(stock_mat, g3_formula(stock__midlen^x, x = 2))
    ok(cmp_code(out, ~(ling_mat__midlen^x)), "Bare formula is treated as default")
})

ok_group("g3_step:stock_iterate", {
    stock <- g3_stock('halibut', 1:10) %>% g3s_age(1,10) %>% g3s_livesonareas(c(x1 = 1, x2 = 2))
    stock__num <- g3_stock_instance(stock)

    ok(cmp_code(rlang::f_rhs(gadget3:::g3_step(~stock_iterate(stock, stock_ss(stock) ))), quote(
        for (halibut__area_idx in seq_along(halibut__areas)) g3_with(
            area := halibut__areas[[halibut__area_idx]],
            for (age in seq(halibut__minage, halibut__maxage, by = 1)) g3_with(
                halibut__age_idx := g3_idx(age - halibut__minage + 1L),
                stock[, halibut__age_idx, halibut__area_idx]))
    )), "By default iterate over area/age")

    ok(cmp_code(rlang::f_rhs(gadget3:::g3_step(~stock_iterate(stock, stock_ss(stock, area = ) ))), quote(
        for (age in seq(halibut__minage, halibut__maxage, by = 1)) g3_with(
            halibut__age_idx := g3_idx(age - halibut__minage + 1L),
            stock[, halibut__age_idx, ])
    )), "Can turn area off")

    ok(cmp_code(rlang::f_rhs(gadget3:::g3_step(~stock_iterate(stock, stock_ssinv(stock, 'area', 'length' ) ))), quote(
        for (halibut__area_idx in seq_along(halibut__areas)) g3_with(
            area := halibut__areas[[halibut__area_idx]],
            for (halibut__length_idx in seq_along(halibut__midlen)) g3_with(
                length := halibut__midlen[[halibut__length_idx]],
                    stock[halibut__length_idx, , halibut__area_idx]))
    )), "Or use stock_ssinv to say what we do want")

    ok(cmp_code(rlang::f_rhs(gadget3:::g3_step(~stock_iterate(stock, stock_ss(stock, length = default) ))), quote(
        for (halibut__area_idx in seq_along(halibut__areas)) g3_with(
            area := halibut__areas[[halibut__area_idx]],
            for (age in seq(halibut__minage, halibut__maxage, by = 1)) g3_with(
                halibut__age_idx := g3_idx(age - halibut__minage + 1L),
                    for (halibut__length_idx in seq_along(halibut__midlen)) g3_with(
                        length := halibut__midlen[[halibut__length_idx]],
                            stock[halibut__length_idx, halibut__age_idx, halibut__area_idx])))
    )), "Can turn length back on & iterate over all dimensions")
})
