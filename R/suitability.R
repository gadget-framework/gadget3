# Suitability functions all return a formula containing the following variables
# * prey_stock__midlen
# * pred_stock__midlen

g3_suitability_exponentiall50 <- function (alpha, l50) {
  f_substitute(~1 / ( 1 + exp(-alpha * (prey_stock__midlen - l50)) ), list(
    alpha = alpha,
    l50 = l50))
}

g3_suitability_andersen <- function (p0, p1, p2, p3 = p4, p4, p5 = ~prey_stock__midlen) {
  # TODO: We need to switch p4 for p3 when log(pred_stock__midlen/p5) - p1 is <= 0
  if (!identical(p3, p4)) stop("p3 not currently supported")
  f_substitute(~p0 + p2 * exp(avoid_zero(-(log(pred_stock__midlen/p5) - p1)**2/p4)), list(
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
  f_substitute(~(prey_stock__midlen/((alpha - 1)*beta*gamma))**(alpha - 1) * exp(alpha -1 - prey_stock__midlen/(beta*gamma)), list(
    alpha = alpha,
    beta = beta, 
    gamma = gamma))
}

g3_suitability_exponential <- function (alpha, beta, gamma, delta) {
  f_substitute(~delta / ( 1 + exp(-alpha - beta * prey_stock__midlen - gamma * pred_stock__midlen)) , list(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    delta = delta))
}  

g3_suitability_straightline <- function(alpha, beta){
  f_substitute(~alpha + beta * prey_stock__midlen, list(alpha = alpha, beta = beta))
}


g3_suitability_constant <- function(alpha){
  alpha
}


g3_suitability_richards <- function(p0,p1,p2,p3,p4){
  f_substitute(~suit_exponential**(1/p4), list(
    suit_exponential = g3_suitability_exponential(p0, p1, p2, p3),
    p4 = p4))
}