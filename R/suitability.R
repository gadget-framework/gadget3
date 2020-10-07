# Suitability functions all return a formula containing the following variables
# * prey_l: prey length(group)
# * pred_l: predator lenghth(group)

g3_suitability_exponentiall50 <- function (alpha, l50) {
  f_substitute(~1 / ( 1 + exp(-alpha * (prey_l - l50)) ), list(
    alpha = alpha,
    l50 = l50))
}

g3_suitability_andersenfleet <- function (p0, p1, p2, p3, p4) {
  f_substitute(~p0 + p2 * exp(logspace_add(-(log(pred_l/prey_l) - p1)**2/p4,0)), list(
    p0 = p0,
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4))
}


g3_suitability_gamma <- function(alpha, beta, gamma){
  ## I'm not sure why beta and gamma are not just a single parameter but 
  ## this is implemented as in gadget2
  f_substitute(~(prey_l/((alpha - 1)*beta*gamma))**(alpha - 1) * exp(alpha -1 - prey_l/(beta*gamma)), list(
    alpha = alpha,
    beta = beta, 
    gamma = gamma))
}

g3_suitability_exponential <- function (alpha, beta, gamma, delta) {
  f_substitute(~delta / ( 1 + exp(-alpha - beta * prey_l - gamma * pred_l)) , list(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    delta = delta))
}  

g3_suitability_straightline <- function(alpha, beta){
  f_substitute(~alpha + beta * prey_l, list(alpha = alpha, beta = beta))
}


g3_suitability_constant <- function(alpha){
  g3_suitability_straightline(alpha,0)
}


g3_suitability_richards <- function(p0,p1,p2,p3,p4){
  f_substitute(~suit_exponential**(1/p4), list(
    suit_exponential = g3_suitability_exponential(p0, p1, p2, p3),
    p4 = p4))
}