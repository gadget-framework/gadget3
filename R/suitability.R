# Suitability functions all return a formula containing the following variables
# * prey_l: prey length(group)
# * pred_l: predator lenghth(group)

g3_suitability_exponentiall50 <- function (alpha, l50) {
    f_substitute(~1 / ( 1 + exp(-alpha * (prey_l - l50)) ), list(
        alpha = alpha,
        l50 = l50))
}
