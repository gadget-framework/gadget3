g3a_grow_lengthvbsimple <- function (linf_f, kappa_f, alpha_f, beta_f) {
    # See src/growthcalc.cc:GrowthCalcH::calcGrowth
    # TODO: Where did alpha_f and beta_f go? Missing weight?
    f_substitute(
        ~(linf_f - stock_meanlen) * (1 - exp(-kappa_f * cur_step_len)),
        list(linf_f = linf_f, kappa_f = kappa_f))
}

g3a_grow_impl_bbinom <- function (beta_f, maxlengthgroupgrowth) {
    ##' @param dmu mean growth for each lengthgroup
    ##' @param lengthgrouplen i.e. dl, the step size for length groups
    ##' @param binn Maximum updating length, i.e. # of length groups
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
        growth.matrix <- array(0,c(na,na))
        for(lg in 1:na){
          if(lg == na){
            growth.matrix[na,na] <- 1
          } else if(lg + n > na){
            growth.matrix[lg,lg:(na-1)] <- val[lg,1:(na - lg )]
            growth.matrix[lg,na] <- sum(val[lg,(na - lg + 1):(n + 1)])
          } else {
            growth.matrix[lg,lg:(n + lg)] <- val[lg,]
          }
        }
        return(growth.matrix)
    }, cpp = '[](vector<Type> dmu, int lengthgrouplen, int binn, Type beta) -> matrix<Type> {
        using namespace Eigen;

        vector<Type> delt_l = dmu / lengthgrouplen;  // i.e. width of length groups
        vector<Type> alpha = (beta * delt_l) / (binn - delt_l);

        // possible length growth
        int na = alpha.size();
        int n = binn;
        alpha = alpha.replicate(n + 1, 1);

        vector<Type> x((n + 1) * na);
        for (auto i = 0; i < x.size(); i++) x(i) = i / alpha.size();

        // Create a probability matrix where the columns represent the
        // probability of growing x lengthgroups for each lengthgroup
        // length group jumps are distributed according to a beta-binomial
        // distribution
        vector<Type> val_vec(na * (n + 1));
        vector<Type> lgamma_arg(na * (n + 1));

        // NB: VECTORIZE1_t-ed lgamma needs a single symbol to work
        val_vec = lgamma((Type) n);
        lgamma_arg = alpha + beta; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = n - x + beta; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = x + alpha; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = n - x + 1; val_vec = val_vec - lgamma(lgamma_arg);
        lgamma_arg = x + 1; val_vec = val_vec - lgamma(lgamma_arg);
        lgamma_arg = n + alpha + beta; val_vec = val_vec - lgamma(lgamma_arg);
        val_vec = val_vec - lgamma(beta);
        // NB: Straight lgamma(alpha) segfaults
        lgamma_arg = alpha + 0; val_vec = val_vec - lgamma(lgamma_arg);

        // Map val_vec into a matrix
        Eigen::Map<Eigen::Matrix<Type, Dynamic, Dynamic>> val(val_vec.vec().data(), na, n);

        matrix<Type> growth_matrix(na, na);
        growth_matrix.setZero();
        for(int lg = 0; lg < na ; lg++) {
          if(lg == (na - 1)){
            growth_matrix(lg, lg) = 1;
          } else if(lg + n > na){
            for (int i = 0 ; i < (na - lg); i++) growth_matrix(lg, lg + i) = val(lg, i);
            growth_matrix(lg, na - 1) = val.block(lg, na - lg, 1, n - (na - lg)).sum();
          } else {
            growth_matrix.row(lg) = val.row(lg);
          }
        }

        return growth_matrix;
    }')

    f_substitute(
        ~growth_bbinom(stock_grow_l, stock_dl, stock_countlen, beta_f),
        list(beta_f = beta_f))
}

g3a_grow <- function(stock, growth_f, impl_f) {
    # See AgeBandMatrix::Grow
    stock_growth_num <- stock_definition(stock, 'stock_num')
    stock_grow_l <- array(dim = dim(stock_growth_num)[[1]])
    # TODO: (gadgetsim) if growth>maxgrowth assume that growth is a bit smaller than maxgrowth
    # TODO: (gadgetsim) if growth is negative assume no growth
    stock_growth_ratio <- matrix(nrow = length(stock_grow_l), ncol = length(stock_grow_l))
    list(
        step055b = stock_step(stock,
            iter = f_substitute(~{
                stock_grow_l <- growth_f
                stock_growth_ratio <- impl_f

                stock_num <- colSums(stock_growth_ratio %*% stock_num)
            }, list(
                growth_f = growth_f,
                impl_f = impl_f))))
}
