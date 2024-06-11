library(unittest)

library(gadget3)

st_vbsimple <- g3_stock('st_vbsimple', seq(50, 100, by = 10)) |> g3s_age(1,5)

actions <- list(
    g3a_time(2000, 2000, step_lengths = c(3, 9)),  # NB: Second step longer than first

    g3a_otherfood(st_vbsimple, 1e6, 1e3),
    g3a_growmature(st_vbsimple, g3a_grow_impl_bbinom(
        g3a_grow_lengthvbsimple(),
        g3a_grow_weightsimple(),
        maxlengthgroupgrowth = 8 )),
    
    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__num$|__wgt$", out_prefix = "late_"),
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

ok_group("Default params") ########
attr(model_fn, 'parameter_template') |>
    g3_init_val("*.Linf", 180) |>
    g3_init_val("*.K", 0.1) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>
    identity() -> params

result <- model_fn(params)
r <- attributes(result)

ok(gadget3:::ut_cmp_df(as.data.frame(r$late_st_vbsimple__num[,,time='2000-01']), '
             age1      age2      age3      age4      age5
50:60    961352.4  961352.4  961352.4  961352.4  961352.4
60:70    964474.8  964474.8  964474.8  964474.8  964474.8
70:80    967582.0  967582.0  967582.0  967582.0  967582.0
80:90    970684.2  970684.2  970684.2  970684.2  970684.2
90:100   973784.3  973784.3  973784.3  973784.3  973784.3
100:Inf 1162122.3 1162122.3 1162122.3 1162122.3 1162122.3
', tolerance = 1e-7), "late_st_vbsimple__num[,,time='2000-01']")
ok(gadget3:::ut_cmp_df(as.data.frame(r$late_st_vbsimple__num[,,time='2000-02']), '
             age1      age2      age3      age4      age5
50:60    886896.4  886896.4  886896.4  886896.4  886896.4
60:70    896034.1  896034.1  896034.1  896034.1  896034.1
70:80    905127.4  905127.4  905127.4  905127.4  905127.4
80:90    914206.1  914206.1  914206.1  914206.1  914206.1
90:100   923278.8  923278.8  923278.8  923278.8  923278.8
100:Inf 1474457.3 1474457.3 1474457.3 1474457.3 1474457.3
', tolerance = 1e-7), "late_st_vbsimple__num[,,time='2000-02']")

ok(gadget3:::ut_cmp_df(as.data.frame(r$late_st_vbsimple__num[,,time='2000-01']), '
             age1      age2      age3      age4      age5
50:60    961352.4  961352.4  961352.4  961352.4  961352.4
60:70    964474.8  964474.8  964474.8  964474.8  964474.8
70:80    967582.0  967582.0  967582.0  967582.0  967582.0
80:90    970684.2  970684.2  970684.2  970684.2  970684.2
90:100   973784.3  973784.3  973784.3  973784.3  973784.3
100:Inf 1162122.3 1162122.3 1162122.3 1162122.3 1162122.3
', tolerance = 1e-7), "late_st_vbsimple__num[,,time='2000-01']")
ok(gadget3:::ut_cmp_df(as.data.frame(r$late_st_vbsimple__num[,,time='2000-02']), '
             age1      age2      age3      age4      age5
50:60    886896.4  886896.4  886896.4  886896.4  886896.4
60:70    896034.1  896034.1  896034.1  896034.1  896034.1
70:80    905127.4  905127.4  905127.4  905127.4  905127.4
80:90    914206.1  914206.1  914206.1  914206.1  914206.1
90:100   923278.8  923278.8  923278.8  923278.8  923278.8
100:Inf 1474457.3 1474457.3 1474457.3 1474457.3 1474457.3
', tolerance = 1e-7), "late_st_vbsimple__num[,,time='2000-02']")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Default params
