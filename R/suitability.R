# Suitability functions all return a formula containing the following variables
# * stock__midlen
# * predator_length

g3_suitability_exponentiall50 <- function (
    alpha = g3_parameterized("alpha", by_stock = by_stock, by_predator = by_predator),
    l50 = g3_parameterized("l50", by_stock = by_stock, by_predator = by_predator),
    by_stock = TRUE,
    by_predator = TRUE) {
  f_substitute(~1 / ( 1 + exp(-alpha * (stock__midlen - l50)) ), list(
    alpha = alpha,
    l50 = l50))
}

g3_suitability_andersen <- function (p0, p1, p2, p3 = p4, p4, p5 = quote(predator_length)) {
  f_substitute(~p0 +
                 avoid_zero(p2) * exp(-(log(avoid_zero_vec(p5/stock__midlen)) - p1)**2/avoid_zero(p3)) *
                 bounded_vec(1000*(p1 - log(avoid_zero_vec(p5/stock__midlen))),0,1) +
                 avoid_zero(p2) * exp(-(log(avoid_zero_vec(p5/stock__midlen)) - p1)**2/avoid_zero(p4)) *
                 bounded_vec(1000*(log(avoid_zero_vec(p5/stock__midlen)) - p1),0,1),
               list(
                 p0 = p0,
                 p1 = p1,
                 p2 = p2,
                 p3 = p3,
                 p4 = p4,
                 p5 = p5))
}

g3_suitability_andersenfleet <- function (
        p0 = g3_parameterized('andersen.p0', value = 0, optimise = FALSE,
                              by_stock = by_stock),
        p1 = g3_parameterized('andersen.p1', value = log(2),
                              by_stock = by_stock, by_predator = by_predator),
        p2 = g3_parameterized('andersen.p2', value = 1, optimise = FALSE,
                              by_stock = by_stock),
        p3 = g3_parameterized('andersen.p3', value = 0.1, exponentiate = exponentiate,
                              by_stock = by_stock, by_predator = by_predator),
        p4 = g3_parameterized('andersen.p4', value = 0.1, exponentiate = exponentiate,
                              by_stock = by_stock, by_predator = by_predator),
        p5 = quote( stock__maxmidlen ),
        by_stock = TRUE,
        by_predator = TRUE,
        exponentiate = TRUE) {
    f_substitute(~p0 +
        p2 * exp(-(log(p5/stock__midlen) - p1)**2/p3) *
        bounded_vec(1000*(p1 - log(p5/stock__midlen)),0,1) +
        p2 * exp(-(log(p5/stock__midlen) - p1)**2/p4) *
        bounded_vec(1000*(log(p5/stock__midlen) - p1),0,1), list(
            p0 = p0,
            p1 = p1,
            p2 = p2,
            p3 = p3,
            p4 = p4,
            p5 = p5))
}

g3_suitability_gamma <- function(alpha, beta, gamma){
  ## I'm not sure why beta and gamma are not just a single parameter but 
  ## this is implemented as in gadget2
  f_substitute(~(stock__midlen/((alpha - 1)*beta*gamma))**(alpha - 1) * exp(alpha -1 - stock__midlen/(beta*gamma)), list(
    alpha = alpha,
    beta = beta, 
    gamma = gamma))
}

g3_suitability_exponential <- function (alpha, beta, gamma, delta) {
  f_substitute(~delta / ( 1 + exp(-alpha - beta * stock__midlen - gamma * predator_length)) , list(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    delta = delta))
}  

g3_suitability_straightline <- function(alpha, beta){
  f_substitute(~alpha + beta * stock__midlen, list(alpha = alpha, beta = beta))
}


g3_suitability_constant <- function(alpha){
  g3_suitability_straightline(alpha, 0)
}


g3_suitability_richards <- function(p0,p1,p2,p3,p4){
  f_substitute(~suit_exponential**(1/p4), list(
    suit_exponential = g3_suitability_exponential(p0, p1, p2, p3),
    p4 = p4))
}