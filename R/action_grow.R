# Returns formula for lengthvbsimple growth function
g3a_grow_lengthvbsimple <- function (linf_f, kappa_f) {
    # See src/growthcalc.cc:GrowthCalcH::calcGrowth
    f_substitute(
        ~((linf_f) - stock__midlen) * (1 - exp(-(kappa_f) * cur_step_size)),
        list(linf_f = linf_f, kappa_f = kappa_f))
}

# Growth half of lengthvbsimple
g3a_grow_weightsimple <- function (alpha_f, beta_f) {
    g3a_grow_weightsimple_vec_rotate <- g3_native(r = function (vec, a) {
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
    g3a_grow_weightsimple_vec_extrude <- g3_native(r = function (vec, a) {
        array(vec, dim = c(length(vec), a))
    }, cpp = '[](vector<Type> vec, int a) -> array<Type> {
        array<Type> out(vec.size(), a);
        for (int i = 0 ; i < vec.size(); i++) {
            for (int j = 0 ; j < a; j++) {
                out(i, j) = vec[i];
            }
        }
        return out;
    }')

    # growmemberfunctions.cc:61 - Make a l --> l' matrix of weight increases,
    # NB: Have to multiply by alpha_f last, otherwise TMB thinks the result should be scalar
    f_substitute(~(
        g3a_grow_weightsimple_vec_rotate(pow_vec(stock__midlen, beta_f), maxlengthgroupgrowth + 1) -
        g3a_grow_weightsimple_vec_extrude(pow_vec(stock__midlen, beta_f), maxlengthgroupgrowth + 1)
    ) * (alpha_f), list(alpha_f = alpha_f, beta_f = beta_f))
}

# Returns bbinom growth implementation formulae
g3a_grow_impl_bbinom <- function (delta_len_f, delta_wgt_f, beta_f, maxlengthgroupgrowth) {
    ##' @param dmu mean growth for each lengthgroup
    ##' @param lengthgrouplen i.e. dl, the step size for length groups
    ##' @param binn Maximum updating length, i.e. # of length groups
    ##' @return lengthgrouplen x (lengthgrouplen + 1) matrix, initial_len -> growth jump
    growth_bbinom <- g3_native(r = function (dmu, lengthgrouplen, binn, beta) {
        # See gadgetsim:R/function.R:growthprob:prob()
        delt_l <- dmu / lengthgrouplen  # i.e. width of length groups
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
    }, cpp = '[](vector<Type> dmu, vector<Type> lengthgrouplen, int binn, Type beta) -> array<Type> {
        using namespace Eigen;

        vector<Type> delt_l = dmu / lengthgrouplen;  // i.e. width of length groups
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
        len = f_substitute(
            ~growth_bbinom(delta_len_f, stock__dl, maxlengthgroupgrowth, beta_f),
            list(
                delta_len_f = delta_len_f,
                maxlengthgroupgrowth = maxlengthgroupgrowth,
                beta_f = beta_f)),
        wgt = f_substitute(
            delta_wgt_f,
            list(
                maxlengthgroupgrowth = maxlengthgroupgrowth)))
}

# Apply a lgroup x lgroup-delta matrix to vector of length groups
# Rows should sum to 1
# - delta_l: Proportion of individuals moving +j length groups
# - delta_w: Weight increase of individuals moving +j length groups
g3a_grow_apply <- g3_native(r = function (delta_l, delta_w, input_num, input_wgt) {
    na <- dim(delta_l)[[1]]  # Number of length groups
    n <- dim(delta_l)[[2]] - 1  # Number of lengthgroup deltas
    # See stockmemberfunctions.cc:121, grow.cc:25

    avoid_zero_vec <- function(a) {
        ( pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))) ) / 1000
    }

    growth.matrix <- array(0,c(na,na))
    wgt.matrix <- array(0,c(na,na))
    for(lg in 1:na){
      if(lg == na) { # Maximum length group can't grow any more
        growth.matrix[na,na] <- sum(delta_l[lg,])
        wgt.matrix[lg,lg:na] <- delta_w[lg,1:(na - lg + 1)]
      } else if(lg + n > na){
        growth.matrix[lg,lg:(na-1)] <- delta_l[lg,1:(na - lg )]
        growth.matrix[lg,na] <- sum(delta_l[lg,(na - lg + 1):(n + 1)])
        wgt.matrix[lg,lg:na] <- delta_w[lg,1:(na - lg + 1)]
      } else {
        growth.matrix[lg,lg:(n + lg)] <- delta_l[lg,]
        wgt.matrix[lg,lg:(n + lg)] <- delta_w[lg,]
      }
    }
    # Apply matrices to stock
    growth.matrix <- growth.matrix * as.vector(input_num)  # NB: Cant matrix-multiply with a 1xn array
    wgt.matrix <- growth.matrix * (wgt.matrix + as.vector(input_wgt))

    # Sum together all length group brackets for both length & weight
    return(array(c(
        Matrix::colSums(growth.matrix),
        Matrix::colSums(wgt.matrix) / avoid_zero_vec(Matrix::colSums(growth.matrix)) ), dim = c(na, 2)))
}, cpp = '[](array<Type> delta_l_ar, array<Type> delta_w_ar, vector<Type> input_num, vector<Type> input_wgt) -> array<Type> {
    // Convert delta_l / delta_w to matrices to get 2 proper dimensions, most of this is row-based.
    matrix<Type> delta_l = delta_l_ar.matrix();
    matrix<Type> delta_w = delta_w_ar.matrix();
    int total_deltas = delta_l.cols();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_l.rows(); // # Length groups

    auto avoid_zero_vec = [](vector<Type> a) -> vector<Type> {
        vector<Type> res(a.size());
        for(int i = 0; i < a.size(); i++) {
            res[i] = logspace_add(a[i] * 1000.0, (Type)0.0) / 1000.0;
        }
        return res;
    };

    matrix<Type> growth_matrix(total_lgs, total_lgs);
    growth_matrix.setZero();
    matrix<Type> weight_matrix(total_lgs, total_lgs);
    weight_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can\'t grow beyond maximum length group
            growth_matrix(lg, lg) = delta_l.row(lg).sum();
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else if(lg + total_deltas > total_lgs) {
            growth_matrix.block(lg, lg, 1, total_lgs - lg) = delta_l.block(lg, 0, 1, total_lgs - lg);
            growth_matrix(lg, total_lgs - 1) = delta_l.row(lg).tail(total_deltas - (total_lgs - lg) + 1).sum();
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else {
            growth_matrix.block(lg, lg, 1, total_deltas) = delta_l.block(lg, 0, 1, total_deltas);
            weight_matrix.block(lg, lg, 1, total_deltas) = delta_w.block(lg, 0, 1, total_deltas);
        }
    }
    // Apply matrices to stock
    // NB: Cast to array to get elementwise multiplication
    growth_matrix = growth_matrix.array().colwise() * input_num.array();
    weight_matrix = (weight_matrix.array().colwise() + input_wgt.array()) * growth_matrix.array();

    // Sum together all length group brackets for both length & weight
    array<Type> combined(total_lgs,2);
    combined.col(0) = growth_matrix.colwise().sum();
    combined.col(1) = weight_matrix.colwise().sum().array().rowwise() / avoid_zero_vec(growth_matrix.colwise().sum()).array().transpose();
    return combined;
}')

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
                     run_at = 5,
                     transition_at = 7) {
    # Drag g3a_grow_apply into scope (we only don't include it here to keep source intelligable)
    g3a_grow_apply <- g3a_grow_apply

    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # See AgeBandMatrix::Grow
    stock__prevtotal <- 0
    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)
    stock__growth_num <- stock_instance(stock)
    stock__growth_l <- array(dim = c(0, 0))  # NB: Dimensions will vary based on impl input
    stock__growth_w <- array(dim = c(0, 0))
    stock__transitioning_num <- stock_instance(stock, 0)
    stock__transitioning_wgt <- stock_instance(stock)

    # TODO: (gadgetsim) if growth>maxgrowth assume that growth is a bit smaller than maxgrowth
    # TODO: (gadgetsim) if growth is negative assume no growth

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

    # If we can, try and only recalc growth when necessary
    calcgrowth_f <- f_substitute(~{
        debug_trace("Calculate length/weight delta matrices for current lengthgroups")
        stock__growth_l <- impl_l_f
        stock__growth_w <- impl_w_f
    }, list(impl_l_f = impl_f$len, impl_w_f = impl_f$wgt))
    # Filter out known constants (hacky but conservative, will fail by being slow)
    growth_dependent_vars <- setdiff(
        all.vars(calcgrowth_f),
        c('stock__growth_l', 'stock__growth_w', 'stock__dl', 'stock__midlen'))
    if (length(growth_dependent_vars) == 0) {
        # Growth is a constant, only do it once
        calcgrowth_f <- f_substitute(~{
            if (cur_time == 0) {
                calcgrowth_f
            }
        }, list(calcgrowth_f = calcgrowth_f))
    } else if (identical(growth_dependent_vars, c('cur_step_size'))) {
        # Growth only dependent on timestep, only recalculate when required
        stock__growth_lastcalc <- -1L
        calcgrowth_f <- f_substitute(~{
            if (stock__growth_lastcalc != floor(cur_step_size * 12L)) {
                calcgrowth_f
                debug_trace("Don't recalculate until cur_step_size changes")
                # NB: stock__growth_lastcalc is integer, cur_step_size is fractional
                stock__growth_lastcalc <- floor(cur_step_size * 12L)
            }
        }, list(calcgrowth_f = calcgrowth_f))
    }

    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_grow for ", stock)

        stock_iterate(stock, if (run_f) {
            calcgrowth_f

            if (strict_mode) stock__prevtotal <- sum(stock_ss(stock__num))

            if (transition_f) g3_with(maturity_ratio, maturity_f, {
                debug_trace("Grow and separate maturing ", stock)
                g3_with(growthresult, g3a_grow_apply(
                        stock__growth_l * maturity_bygrowth, stock__growth_w,
                        stock_ss(stock__num) * maturity_bylength, stock_ss(stock__wgt)), {
                    stock_ss(stock__transitioning_num) <- growthresult[,g3_idx(1)]
                    stock_ss(stock__transitioning_wgt) <- growthresult[,g3_idx(2)]
                })

                debug_trace("Grow non-maturing ", stock)
                g3_with(growthresult, g3a_grow_apply(
                        stock__growth_l * invmaturity_bygrowth, stock__growth_w,
                        stock_ss(stock__num) * invmaturity_bylength, stock_ss(stock__wgt)), {
                    stock_ss(stock__num) <- growthresult[,g3_idx(1)]
                    stock_ss(stock__wgt) <- growthresult[,g3_idx(2)]
                })
            }) else {
                debug_trace("Update ", stock, " using delta matrices")
                g3_with(growthresult, g3a_grow_apply(
                        stock__growth_l, stock__growth_w,
                        stock_ss(stock__num), stock_ss(stock__wgt)), {
                    stock_ss(stock__num) <- growthresult[,g3_idx(1)]
                    stock_ss(stock__wgt) <- growthresult[,g3_idx(2)]
                })
            }

            if (strict_mode) {
                if (transition_f) {
                    stopifnot(abs(stock__prevtotal - sum(stock_ss(stock__num)) - sum(stock_ss(stock__transitioning_num))) < 0.0001)
                } else {
                    stopifnot(abs(stock__prevtotal - sum(stock_ss(stock__num))) < 0.0001)
                }
            }
        })
    }, list(
            run_f = run_f,
            calcgrowth_f = calcgrowth_f,
            maturity_f = maturity_f,
            # NB: Apply maturity in different places, depending if stock__growth_l is mentioned in formluae (and thus a len x deltal matrix)
            maturity_bylength = if ("stock__growth_l" %in% all.vars(maturity_f)) 1 else quote(maturity_ratio),
            maturity_bygrowth = if ("stock__growth_l" %in% all.vars(maturity_f)) quote(maturity_ratio) else 1,
            invmaturity_bylength = if ("stock__growth_l" %in% all.vars(maturity_f)) 1 else quote((1 - maturity_ratio)),
            invmaturity_bygrowth = if ("stock__growth_l" %in% all.vars(maturity_f)) quote((1 - maturity_ratio)) else 1,
            transition_f = transition_f)))
    return(as.list(out))
}
