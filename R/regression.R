regression_linear <- g3_native(r = function (N, I, fixed_intercept, fixed_slope) {
    # TRUE iff value is finite in both vectors
    finites <- is.finite(N) & is.finite(I)
    meanI <- mean(I[finites])
    meanN <- mean(N[finites])

    if (is.finite(fixed_slope)) {
        slope <- fixed_slope
    } else {
        Imd <- I[finites] - meanI
        Nmd <- N[finites] - meanN
        slope <- sum(Imd * Nmd) / avoid_zero(sum(Nmd**2))
    }
    if (is.finite(fixed_intercept)) {
        intercept <- fixed_intercept
    } else {
        intercept <- meanI - slope * meanN
    }
    
    return(c(
        nll =  sum((intercept + slope * N[finites] - I[finites])**2),
        intercept = intercept,
        slope = slope ))
}, cpp = '[&avoid_zero](vector<Type> N, vector<Type> I, Type fixed_intercept, Type fixed_slope) -> vector<Type> {
    // TRUE iff value is finite in both vectors
    Eigen::Matrix<bool, Eigen::Dynamic, 1> finites = N.isFinite() && I.isFinite();

    Type meanI = finites.select(I, 0).sum() / finites.count();
    Type meanN = finites.select(N, 0).sum() / finites.count();

    Type intercept, slope;
    if (std::isfinite(asDouble(fixed_slope))) {
        slope = fixed_slope;
    } else {
        slope = (finites.select(I - meanI, 0) * finites.select(N - meanN, 0)).sum()
             / avoid_zero(finites.select(N - meanN, 0).pow(2).sum());
    }
    if (std::isfinite(asDouble(fixed_intercept))) {
        intercept = fixed_intercept;
    } else {
        intercept = meanI - slope * meanN;
    }

    vector<Type> out(3);
    out(0) = (intercept + slope * finites.select(N, 0) - finites.select(I, intercept)).pow(2).sum();
    out(1) = intercept;
    out(2) = slope;
    return out;
}', depends = c('avoid_zero'))
