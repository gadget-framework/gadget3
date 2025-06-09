g3a_grow_vec_rotate <- g3_native(r = function (vec, a) {
    out <- vapply(
        seq_len(a),  # 0..maxlengthgrouplen increases
        function (i) vec[i:(i+length(vec) - 1)],
        numeric(length(vec)))
    out[is.na(out)] <- vec[length(vec)]  # Overflowed entries should be capped at the final one
    out
}, cpp = '[](vector<Type> vec, int a) -> array<Type> {
    array<Type> out(vec.size(), a);
    for (int i = 0 ; i < vec.size(); i++) {
        for (int j = 0 ; j < a; j++) {
            out(i, j) = vec(j + i < vec.size() ? j + i : vec.size() - 1);
        }
    }
    return out;
}')
g3a_grow_vec_extrude <- g3_native(r = function (vec, a) {
    array(vec, dim = c(length(vec), a))
}, cpp = '[](vector<Type> vec, int a) -> array<Type> {
    array<Type> out(vec.size(), a);
    out = vec.replicate(a, 1);
    return out;
}')

# Returns formula for lengthvbsimple growth function
g3a_grow_lengthvbsimple <- function (
        linf_f = g3_parameterized('Linf', by_stock = by_stock),
        kappa_f = g3_parameterized('K', by_stock = by_stock),
        by_stock = TRUE) {
    # See src/growthcalc.cc:GrowthCalcH::calcGrowth
    # NB: avoid_zero() converts negative growth into zero-growth, due to
    #     https://github.com/gadget-framework/gadget3/issues/18
    #     but zero-growth is a valid result here
    f_substitute(
        ~avoid_zero((linf_f - stock__midlen) * (1 - exp(-(kappa_f) * cur_step_size))),
        list(linf_f = linf_f, kappa_f = kappa_f))
}

# Growth half of lengthvbsimple
g3a_grow_weightsimple <- function (
        alpha_f = g3_parameterized('walpha', by_stock = by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = by_stock),
        by_stock = TRUE) {
    g3a_grow_vec_rotate <- g3a_grow_vec_rotate
    g3a_grow_vec_extrude <- g3a_grow_vec_extrude

    # growmemberfunctions.cc:61 - Make a l --> l' matrix of weight increases,
    # NB: Have to multiply by alpha_f last, otherwise TMB thinks the result should be scalar
    f_substitute(~(
        g3a_grow_vec_rotate(stock__midlen^beta_f, maxlengthgroupgrowth + 1) -
        g3a_grow_vec_extrude(stock__midlen^beta_f, maxlengthgroupgrowth + 1)
    ) * (alpha_f), list(alpha_f = alpha_f, beta_f = beta_f))
}

g3a_grow_length_multspec <- function(
        p0 = g3_parameterized('multispec.p0', value = 1, by_stock = by_stock),
        p1 = g3_parameterized('multispec.p1', value = 1, by_stock = by_stock),
        p2 = g3_parameterized('multispec.p2', value = 1, by_stock = by_stock),
        p3 = g3_parameterized('multispec.p3', value = 0, by_stock = by_stock),
        temperature = 0,
        by_stock = TRUE) {
    ~cur_step_size * p0 * stock__midlen^p1 * stock_ss(stock__feedinglevel) * (p2 * temperature + p3)
}

g3a_grow_weight_multspec <- function(
        p4 = g3_parameterized('multispec.p4', value = 1, by_stock = by_stock),
        p5 = g3_parameterized('multispec.p5', value = 1, by_stock = by_stock),
        p6 = g3_parameterized('multispec.p6', value = 0, by_stock = by_stock),
        p7 = g3_parameterized('multispec.p7', value = 1, by_stock = by_stock),
        p8 = g3_parameterized('multispec.p8', value = 0, by_stock = by_stock),
        temperature = 0,
        by_stock = TRUE) {
    g3a_grow_vec_extrude <- g3a_grow_vec_extrude

    ~g3a_grow_vec_extrude(
        cur_step_size *
        p4 *
        (stock_ss(stock__wgt))^p5 *
        (stock_ss(stock__feedinglevel) - p6) *
        (p7 * temperature + p8), maxlengthgroupgrowth + 1)
}

g3a_grow_length_weightjones <- function(
        p0 = g3_parameterized('weightjones.p0', value = 0, by_stock = by_stock),
        p1 = g3_parameterized('weightjones.p1', value = 0, by_stock = by_stock),
        p2 = g3_parameterized('weightjones.p2', value = 1, by_stock = by_stock),
        p3 = g3_parameterized('weightjones.p3', value = 0, by_stock = by_stock),
        p4 = g3_parameterized('weightjones.p4', value = 1, by_stock = by_stock),
        p5 = g3_parameterized('weightjones.p5', value = 100, by_stock = by_stock),
        p6 = g3_parameterized('weightjones.p6', value = 1, by_stock = by_stock),
        p7 = g3_parameterized('weightjones.p7', value = 1, by_stock = by_stock),
        reference_weight = 0,
        temperature = 0,
        by_stock = TRUE) {
    # https://github.com/gadget-framework/gadget2/blob/master/src/growthcalc.cc#L311
    r <- ~(
              stock_ss(stock__wgt) -
              (p0 + stock_ss(stock__feedinglevel) * (p1 + p2 * stock_ss(stock__feedinglevel))) * reference_weight
          ) / stock_ss(stock__wgt)  # No, this is not a regex /
    # NB: All of growth_delta_w is equal, as it doesn't depend on length
    ~dif_pminmax(p3 + p4 * r, 0, p5, 1e5) * growth_delta_w[,1] / (p6 * p7 * stock__midlen^(p7 - 1))
}

g3a_grow_weight_weightjones <- function(
        q0 = g3_parameterized('weightjones.q0', value = 1, by_stock = by_stock),
        q1 = g3_parameterized('weightjones.q1', value = 1, by_stock = by_stock),
        q2 = g3_parameterized('weightjones.q2', value = 1, by_stock = by_stock),
        q3 = g3_parameterized('weightjones.q3', value = 1, by_stock = by_stock),
        q4 = g3_parameterized('weightjones.q4', value = 1, by_stock = by_stock),
        q5 = g3_parameterized('weightjones.q5', value = 0, by_stock = by_stock),
        max_consumption = g3a_predate_maxconsumption(temperature = temperature),
        temperature = 0,
        by_stock = TRUE) {
    # Convert max_consumption formula to use stock__midlen
    max_consumption <- call_replace(
        max_consumption,
        predator_length = function(x) quote( stock__midlen ),
        predstock = function(x) quote( stock ),
        end = NULL )
    # NB: Pretty easy to have negative growth with nonsense parameters, thus avoid_zero()
    ~g3a_grow_vec_extrude(avoid_zero(cur_step_size * (
        ((max_consumption * stock_ss(stock__feedinglevel)) / (q0 * avoid_zero(stock_ss(stock__wgt))^q1)) -
        q2 * (stock_ss(stock__wgt))^q3 * exp(q4 * temperature + q5) )), maxlengthgroupgrowth + 1)
}

# Returns bbinom growth implementation formulae
g3a_grow_impl_bbinom <- function (
        delta_len_f = g3a_grow_lengthvbsimple(by_stock = by_stock),
        delta_wgt_f = g3a_grow_weightsimple(by_stock = by_stock),
        beta_f = g3_parameterized('bbin', by_stock = by_stock),
        maxlengthgroupgrowth,
        by_stock = TRUE) {
    maxlengthgroupgrowth <- as.integer(maxlengthgroupgrowth)
    ##' @param delt_l Vector, for each lengthgroup, mean # of length groups to grow by
    ##' @param binn Maximum possible number of length groups to grow by
    ##' @return length(delt_l) x (length(delt_l) + 1) 2-dimensional array, initial_len -> growth jump
    growth_bbinom <- g3_native(r = function (delt_l, binn, beta) {
        # See gadgetsim:R/function.R:growthprob:prob()
        # binn - delt_l ==> (maxlengthgroupgrowth) - (current desired lengthgroup jump)
        alpha <- (beta * delt_l) / (binn - delt_l)

        ## possible length growth
        x <- 0:binn

        na <- length(alpha)
        n <- length(x) - 1
        alpha <- rep(alpha,n + 1)
        x <- rep(x,each=na)
        ## Create a probability matrix where the columns represent the
        ## probability of growing x lengthgroups for each lengthgroup
        ## length group jumps are distributed according to a beta-binomial
        ## distribution
        val <- exp(lgamma(n + 1)+
                   lgamma(alpha + beta) +
                   lgamma(n - x + beta) +
                   lgamma(x + alpha) -
                   lgamma(n - x + 1) -
                   lgamma(x + 1) -
                   lgamma(n + alpha + beta) -
                   lgamma(beta) -
                   lgamma(alpha))
        dim(val) <- c(na,n + 1)
        return(val)
    }, cpp = '[](vector<Type> delt_l, int binn, Type beta) -> array<Type> {
        using namespace Eigen;

        vector<Type> alpha_1 = (beta * delt_l) / (binn - delt_l);

        // possible length growth
        int na = alpha_1.size();
        int n = binn;

        vector<Type> alpha(na * (n + 1));
        // TODO: alpha.replicate(n + 1, 1) should do this, but first entry is zero?
        for (auto i = 0; i < alpha.size(); i++) alpha(i) = alpha_1(i % alpha_1.size());

        vector<Type> x((n + 1) * na);
        for (auto i = 0; i < x.size(); i++) x(i) = i / na;

        // Create a probability matrix where the columns represent the
        // probability of growing x lengthgroups for each lengthgroup
        // length group jumps are distributed according to a beta-binomial
        // distribution
        array<Type> val(na, n + 1);
        val = (lgamma((Type) n + 1) +
            lgamma((vector<Type>)(alpha + beta)) +
            lgamma((vector<Type>)(n - x + beta)) +
            lgamma((vector<Type>)(x + alpha)) -
            lgamma((vector<Type>)(n - x + 1)) -
            lgamma((vector<Type>)(x + 1)) -
            lgamma((vector<Type>)(n + alpha + beta)) -
            lgamma(beta) -
            lgamma(alpha)).exp();
        return(val);
    }')

    list(
        delta_dim = seq(0, maxlengthgroupgrowth),
        len = f_substitute(
            # NB: avoid_zero() means zero-growth doesn't result in NaN
            # NB: We convert delta_len_f into # of length groups to jump, but badly by assuming length groups
            #     are evenly sized
            ~growth_bbinom(avoid_zero((delta_len_f) / stock__plusdl), maxlengthgroupgrowth, avoid_zero(beta_f)),
            list(
                delta_len_f = delta_len_f,
                maxlengthgroupgrowth = maxlengthgroupgrowth,
                beta_f = beta_f)),
        wgt = f_substitute(
            delta_wgt_f,
            list(
                maxlengthgroupgrowth = maxlengthgroupgrowth)))
}


g3a_grow_matrix_len <- g3_native(r = function (delta_l) {
    na <- dim(delta_l)[[1]]  # Number of length groups
    n <- dim(delta_l)[[2]] - 1  # Number of lengthgroup deltas
    # See stockmemberfunctions.cc:121, grow.cc:25

    growth.matrix <- array(0,c(na,na))
    for(lg in 1:na){
      if(lg == na) { # Maximum length group can't grow any more
        growth.matrix[na,na] <- sum(delta_l[lg,])
      } else if(lg + n > na){
        growth.matrix[lg,lg:(na-1)] <- delta_l[lg,1:(na - lg )]
        growth.matrix[lg,na] <- sum(delta_l[lg,(na - lg + 1):(n + 1)])
      } else {
        growth.matrix[lg,lg:(n + lg)] <- delta_l[lg,]
      }
    }
    return(growth.matrix)
}, cpp = '[](array<Type> delta_l_ar) -> matrix<Type> {
    // Convert delta_l / delta_w to matrices to get 2 proper dimensions, most of this is row-based.
    matrix<Type> delta_l = delta_l_ar.matrix();
    int total_deltas = delta_l.cols();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_l.rows(); // # Length groups

    matrix<Type> growth_matrix(total_lgs, total_lgs);
    growth_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can\'t grow beyond maximum length group
            growth_matrix(lg, lg) = delta_l.row(lg).sum();
        } else if(lg + total_deltas > total_lgs) {
            growth_matrix.block(lg, lg, 1, total_lgs - lg) = delta_l.block(lg, 0, 1, total_lgs - lg);
            growth_matrix(lg, total_lgs - 1) = delta_l.row(lg).tail(total_deltas - (total_lgs - lg) + 1).sum();
        } else {
            growth_matrix.block(lg, lg, 1, total_deltas) = delta_l.block(lg, 0, 1, total_deltas);
        }
    }
    return(growth_matrix);
}')

g3a_grow_matrix_wgt <- g3_native(r = function (delta_w) {
    na <- dim(delta_w)[[1]]  # Number of length groups
    n <- dim(delta_w)[[2]] - 1  # Number of lengthgroup deltas
    # See stockmemberfunctions.cc:121, grow.cc:25

    wgt.matrix <- array(0,c(na,na))
    for(lg in 1:na){
      if(lg == na) { # Maximum length group can't grow any more
        wgt.matrix[lg,lg:na] <- delta_w[lg,1:(na - lg + 1)]
      } else if(lg + n > na){
        wgt.matrix[lg,lg:na] <- delta_w[lg,1:(na - lg + 1)]
      } else {
        wgt.matrix[lg,lg:(n + lg)] <- delta_w[lg,]
      }
    }
    return(wgt.matrix)
}, cpp = '[](array<Type> delta_w_ar) {
    // Convert delta_l / delta_w to matrices to get 2 proper dimensions, most of this is row-based.
    matrix<Type> delta_w = delta_w_ar.matrix();
    int total_deltas = delta_w.cols();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_w.rows(); // # Length groups

    matrix<Type> weight_matrix(total_lgs, total_lgs);
    weight_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can\'t grow beyond maximum length group
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else if(lg + total_deltas > total_lgs) {
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else {
            weight_matrix.block(lg, lg, 1, total_deltas) = delta_w.block(lg, 0, 1, total_deltas);
        }
    }
    return(weight_matrix);
}')

g3a_grow_apply <- g3_native(r = function (growth.matrix, wgt.matrix, input_num, input_wgt) {
    na <- dim(growth.matrix)[[1]]  # Number of length groups
    # See stockmemberfunctions.cc:121, grow.cc:25

    # Apply matrices to stock
    growth.matrix <- growth.matrix * as.vector(input_num)  # NB: Cant matrix-multiply with a 1xn array
    wgt.matrix <- growth.matrix * (wgt.matrix + as.vector(input_wgt))

    # Sum together all length group brackets for both length & weight
    growth.matrix.sum <- colSums(growth.matrix)
    return(array(c(
        growth.matrix.sum,
        colSums(wgt.matrix) / avoid_zero(growth.matrix.sum) ), dim = c(na, 2)))
}, cpp = '[](matrix<Type> growth_matrix, matrix<Type> weight_matrix, vector<Type> input_num, vector<Type> input_wgt) -> array<Type> {
    int total_lgs = growth_matrix.cols(); // # Length groups

    // Apply matrices to stock
    // NB: Cast to array to get elementwise multiplication
    growth_matrix = growth_matrix.array().colwise() * input_num.array();
    weight_matrix = (weight_matrix.array().colwise() + input_wgt.array()) * growth_matrix.array();

    // Sum together all length group brackets for both length & weight
    array<Type> combined(total_lgs,2);
    combined.col(0) = growth_matrix.colwise().sum();
    combined.col(1) = weight_matrix.colwise().sum().array().rowwise() / avoid_zero(growth_matrix.colwise().sum()).array().transpose();
    return combined;
}', depends = c("avoid_zero"))

# Combined growth / maturity step for a stock
# - impl_f: formulae for growth implmentation, e.g. g3a_grow_impl_bbinom()
# - transition_f: formula producing TRUE/FALSE for when maturity should also be run
g3a_growmature <- function(stock,
                     impl_f,
                     maturity_f = ~0,
                     output_stocks = list(),
                     output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)),
                     transition_f = ~cur_step_final,
                     run_f = ~TRUE,
                     run_at = g3_action_order$grow,
                     transition_at = g3_action_order$mature) {
    # Drag g3a_grow_apply into scope (we only don't include it here to keep source intelligable)
    g3a_grow_apply <- g3a_grow_apply
    g3a_grow_matrix_len <- g3a_grow_matrix_len
    g3a_grow_matrix_wgt <- g3a_grow_matrix_wgt

    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # See AgeBandMatrix::Grow
    stock__prevtotal <- 0
    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)
    stock__growth_num <- g3_stock_instance(stock)
    stock__growth_l <- array(
        dim = c(length = stock$dim$length, delta = length(impl_f$delta_dim)),
        dimnames = list(length = stock$dimnames$length, delta = impl_f$delta_dim))
    stock__growth_w <- array(
        dim = c(length = stock$dim$length, delta = length(impl_f$delta_dim)),
        dimnames = list(length = stock$dimnames$length, delta = impl_f$delta_dim))
    stock__transitioning_num <- g3_stock_instance(stock, 0)
    stock__transitioning_wgt <- g3_stock_instance(stock)

    # Add transition steps if output_stocks provided
    if (length(output_stocks) == 0) {
        # NB: This will ensure all maturity code is thrown away below
        transition_f <- FALSE
    } else {
        out[[step_id(run_at, stock)]] <- g3_step(f_substitute(~if (transition_f) {
            debug_trace("Reset transitioning arrays")
            stock_with(stock, stock__transitioning_num[] <- 0)
            stock_with(stock, stock__transitioning_wgt[] <- stock__wgt[])
        }, list(
            transition_f = transition_f)))
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock, output_stocks, output_ratios, run_f = transition_f)
    }

    # Set local growth_delta_(x) based on global value, recalculating if necessary
    growth_delta_l <- f_substitute(~stock_with(stock, (stock__growth_l[] <- f)), list(f = impl_f$len))
    growth_delta_w <- f_substitute(~stock_with(stock, (stock__growth_w[] <- f)), list(f = impl_f$wgt))
    stock__growth_lastcalc = -1L
    post_growth_f <- quote({})

    # If we can, try and only recalc growth when necessary
    growth_dependent_vars <- sort(setdiff(
        c(all.vars(growth_delta_l), all.vars(growth_delta_w)),
        # Filter out known constants (hacky but conservative, will fail by being slow)
        c('stock', 'stock__growth_l', 'stock__growth_w', 'stock__dl', 'stock__plusdl', 'stock__midlen')))
    if (length(growth_dependent_vars) == 0) {
        # Growth is a constant, only do it once
        growth_delta_l <- f_substitute(quote(if (cur_time > 0) stock__growth_l else growth_delta_l), list(growth_delta_l = growth_delta_l))
        growth_delta_w <- f_substitute(quote(if (cur_time > 0) stock__growth_w else growth_delta_w), list(growth_delta_w = growth_delta_w))
    } else if (identical(growth_dependent_vars, c('cur_step_size'))) {
        # Growth only dependent on timestep, only recalculate when required
        growth_delta_l <- f_substitute(~stock_with(stock, if (stock__growth_lastcalc == floor(cur_step_size * 12L)) stock__growth_l else growth_delta_l), list(growth_delta_l = growth_delta_l))
        growth_delta_w <- f_substitute(~stock_with(stock, if (stock__growth_lastcalc == floor(cur_step_size * 12L)) stock__growth_w else growth_delta_w), list(growth_delta_w = growth_delta_w))
        post_growth_f <- quote( stock_with(stock, stock__growth_lastcalc <- floor(cur_step_size * 12L)) )
    } else if (identical(growth_dependent_vars, c('cur_step_size', 'cur_year'))) {
        # Growth dependent on timestep/year, only recalculate when required
        growth_delta_l <- f_substitute(~stock_with(stock, if (stock__growth_lastcalc == floor(cur_step_size * 12L) * 10000L + cur_year) stock__growth_l else growth_delta_l), list(growth_delta_l = growth_delta_l))
        growth_delta_w <- f_substitute(~stock_with(stock, if (stock__growth_lastcalc == floor(cur_step_size * 12L) * 10000L + cur_year) stock__growth_w else growth_delta_w), list(growth_delta_w = growth_delta_w))
        post_growth_f <- quote( stock_with(stock, stock__growth_lastcalc <- floor(cur_step_size * 12L) * 10000L + cur_year) )
    }

    # Formulae to calculate growth matrices: We're separating these to happen outside age loops if possible
    growthmat_l <- ~g3a_grow_matrix_len(growth_delta_l)
    growthmat_w <- ~g3a_grow_matrix_wgt(growth_delta_w)

    # Rename maturity_f for below
    maturity_ratio <- maturity_f

    # NB: Apply maturity in different places, depending if stock__growth_l is mentioned in formluae (and thus a len x deltal matrix)
    if ("growth_delta_l" %in% all.vars(maturity_f)) {
        growthimmresult <- ~g3a_grow_apply(
            # Recalculate growth matrix each time, using maturitybygrowth result
            g3a_grow_matrix_len(growth_delta_l * (1 - maturity_ratio)),
            growthmat_w,
            stock_ss(stock__num),
            stock_ss(stock__wgt) )
        growthmatresult <- ~g3a_grow_apply(
            g3a_grow_matrix_len(growth_delta_l * maturity_ratio),
            growthmat_w,
            stock_ss(stock__num),
            stock_ss(stock__wgt) )
        # i.e. when transition_f is FALSE
        growthresult <- ~g3a_grow_apply(
            g3a_grow_matrix_len(growth_delta_l),
            growthmat_w,
            stock_ss(stock__num),
            stock_ss(stock__wgt) )
    } else {
        growthimmresult <- ~g3a_grow_apply(
            # Recalculate growth matrix each time, using maturitybygrowth result
            growthmat_l,
            growthmat_w,
            stock_ss(stock__num) * (1 - maturity_ratio),
            stock_ss(stock__wgt) )
        growthmatresult <- ~g3a_grow_apply(
            growthmat_l,
            growthmat_w,
            stock_ss(stock__num) * maturity_ratio,
            stock_ss(stock__wgt) )
        # i.e. when transition_f is FALSE
        growthresult <- ~g3a_grow_apply(
            # Recalculate growth matrix each time, using maturitybygrowth result
            growthmat_l,
            growthmat_w,
            stock_ss(stock__num),
            stock_ss(stock__wgt) )
    }

    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_grow for ", stock)

        stock_iterate(stock, if (run_f) {
            if (strict_mode) stock__prevtotal <- sum(stock_ss(stock__num))

            if (transition_f) {
                debug_trace("Grow and separate maturing ", stock)
                stock_ss(stock__transitioning_num) <- growthmatresult[,g3_idx(1)]
                stock_ss(stock__transitioning_wgt) <- growthmatresult[,g3_idx(2)]
                debug_trace("Grow non-maturing ", stock)
                stock_ss(stock__num) <- growthimmresult[,g3_idx(1)]
                stock_ss(stock__wgt) <- growthimmresult[,g3_idx(2)]
            } else {
                debug_trace("Update ", stock, " using delta matrices")
                stock_ss(stock__num) <- growthresult[,g3_idx(1)]
                stock_ss(stock__wgt) <- growthresult[,g3_idx(2)]
            }

            if (strict_mode) {
                if (transition_f) {
                    stock_assert(
                        abs(stock__prevtotal - sum(stock_ss(stock__num)) - sum(stock_ss(stock__transitioning_num))) < 0.0001,
                        "g3a_growmature: ", stock, "__num totals are not the same before and after growth (excluding maturation)")
                } else {
                    stock_assert(
                        abs(stock__prevtotal - sum(stock_ss(stock__num))) < 0.0001,
                        "g3a_growmature: ", stock, "__num totals are not the same before and after growth")
                }
            }
        })
        post_growth_f
    }, list(
            run_f = run_f,
            post_growth_f = post_growth_f,
            maturity_f = maturity_f,
            transition_f = transition_f)))
    return(as.list(out))
}
