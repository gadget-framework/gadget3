# Returns formula for lengthvbsimple growth function
g3a_grow_lengthvbsimple <- function (linf_f, kappa_f, alpha_f, beta_f) {
    # See src/growthcalc.cc:GrowthCalcH::calcGrowth
    delta_len_f <- f_substitute(
        ~(linf_f - stock__meanlen) * (1 - exp(-kappa_f * cur_step_len)),
        list(linf_f = linf_f, kappa_f = kappa_f))
    delta_wgt_f <- f_substitute(
        ~alpha_f * ( (stock__meanlen - delta_len_f)^beta_f - stock__meanlen^beta_f ),
        list(alpha_f = alpha_f, beta_f = beta_f, delta_len_f = delta_len_f))
    list(len = delta_len_f, wgt = delta_wgt_f)
}

# Returns bbinom growth implementation formulae
g3a_grow_impl_bbinom <- function (beta_f, maxlengthgroupgrowth) {
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
    }, cpp = '[](vector<Type> dmu, int lengthgrouplen, int binn, Type beta) -> array<Type> {
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
        for (auto i = 0; i < x.size(); i++) x(i) = i / n;

        // Create a probability matrix where the columns represent the
        // probability of growing x lengthgroups for each lengthgroup
        // length group jumps are distributed according to a beta-binomial
        // distribution
        vector<Type> val_vec(na * (n + 1));
        vector<Type> lgamma_arg(na * (n + 1));

        // NB: VECTORIZE1_t-ed lgamma needs a single symbol to work
        val_vec = lgamma((Type) n + 1);
        lgamma_arg = alpha + beta; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = n - x + beta; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = x + alpha; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = n - x + 1; val_vec = val_vec - lgamma(lgamma_arg);
        lgamma_arg = x + 1; val_vec = val_vec - lgamma(lgamma_arg);
        lgamma_arg = n + alpha + beta; val_vec = val_vec - lgamma(lgamma_arg);
        val_vec = val_vec - lgamma(beta);
        // NB: Straight lgamma(alpha) segfaults
        lgamma_arg = alpha + 0; val_vec = val_vec - lgamma(lgamma_arg);
        val_vec = val_vec.exp();

        // Map val_vec into a matrix
        array<Type> val(na, n + 1);
        val = val_vec;
        return(val);
    }')

    f_substitute(
        ~growth_bbinom(stock__grow_f, stock__dl, stock__countlen, beta_f),
        list(beta_f = beta_f))
}

# Growth step for a stock
# - growth_f: formulae for growth, e.g. g3a_grow_lengthvbsimple()
# - impl_f: formulae for growth implmentation, e.g. g3a_grow_impl_bbinom()
g3a_grow <- function(stock, growth_f, impl_f) {
    # See AgeBandMatrix::Grow
    stock__growth_num <- stock_definition(stock, 'stock__num')
    stock__growth_l <- array(dim = c(0, 0))  # NB: Dimensions will vary based on impl input
    stock__growth_w <- array(dim = dim(stock__growth_num)[[1]])

    # TODO: (gadgetsim) if growth>maxgrowth assume that growth is a bit smaller than maxgrowth
    # TODO: (gadgetsim) if growth is negative assume no growth

    # Apply a lgroup x lgroup-delta matrix to vector of length groups
    g3a_grow_apply <- g3_native(r = function (lg_deltas, input_num) {
        na <- dim(lg_deltas)[[1]]
        n <- dim(lg_deltas)[[2]] - 1

        growth.matrix <- array(0,c(na,na))
        for(lg in 1:na){
          if(lg == na){
            growth.matrix[na,na] <- 1
          } else if(lg + n > na){
            growth.matrix[lg,lg:(na-1)] <- lg_deltas[lg,1:(na - lg )]
            growth.matrix[lg,na] <- sum(lg_deltas[lg,(na - lg + 1):(n + 1)])
          } else {
            growth.matrix[lg,lg:(n + lg)] <- lg_deltas[lg,]
          }
        }
        return(Matrix::colSums(growth.matrix * input_num))
    }, cpp = '[](array<Type> lg_deltas, vector<Type> input_num) -> vector<Type> {
        lg_deltas = lg_deltas.transpose();
        int total_deltas = lg_deltas.rows();  // # Length group increases (should be +1 for the no-change group)
        int total_lgs = lg_deltas.cols(); // # Length groups
        vector<Type> lg_growth;
        vector<Type> out;

        lg_growth.resize(total_lgs);
        out.resize(total_lgs);
        out.setZero();
        for (int lg = 0; lg < total_lgs; lg++) {
          // Cant shrink
          lg_growth.head(lg) = 0;
          // Add any that have an appropriate group
          lg_growth.tail(total_lgs - lg) = lg_deltas.col(lg).head(total_lgs - lg);
          if (total_deltas - (total_lgs - lg) > 0) {
              // Add any remaining to plus-group
              lg_growth.tail(1) += lg_deltas.col(lg).tail(total_deltas - (total_lgs - lg)).sum();
          }
          out += lg_growth * input_num(lg);
        }
        return out;
    }')

    out <- list()
    out[[paste0("050:", stock$name)]] <- stock_step(f_substitute(~{
        stock_comment("g3a_grow for ", stock)
        stock_iterate(stock, {
            comment("Calculate increase in length/weight for each lengthgroup")
            stock__growth_l <- impl_l_f
            stock__growth_w <- growth_w_f

            stock__wgt[stock__iter] <- stock__wgt[stock__iter] * stock__num[stock__iter]  # Convert to total weight
            stock__num[stock__iter] <- g3a_grow_apply(stock__growth_l, stock__num[stock__iter])
            stock__wgt[stock__iter] <- (stock__wgt[stock__iter] + stock__growth_w) / pmax(stock__num[stock__iter], 0.00001)  # Add extra weight, back to mean
        })
    }, list(
            impl_l_f = f_substitute(impl_f, list(stock__grow_f = growth_f$len)),
            growth_w_f = growth_f$wgt)))
    return(out)
}
