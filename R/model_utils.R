g3a_initial_abund <- function(scalar, 
                              init, 
                              M, 
                              init_F,
                              minage = 0,
                              p_age = 1){
  gadget3:::f_substitute(
    ~scalar * init * exp(-1 * (M + init_F) * 
                           age - minage) * p_age,
    list(scalar = scalar, 
         init = init, 
         M = M, 
         init_F = init_F,
         minage = minage,
         p_age)
  )
  
}

g3a_initial_vonb <- function(Linf, K, recl, K_scale = 0.001){
  gadget3:::f_substitute(
    ~Linf * (1 - exp(-1 * K_scale * K * (age - (1 + log(1 - recl/Linf)/K)))),
    list(K_scale = K_scale,
         Linf = Linf,
         K = K,
         recl = recl))
  
}

g3a_initial_ageprop <- function(alpha, a50){
  gadget3:::f_substitute(
    ~bounded(-1*alpha*(age - a50),0,1),
    list(alpha = alpha, a50 = a50))
}

g3a_initial_sigma <- function(alpha, beta, gamma, mean_l){
  gadget3:::f_substitute(
    ~mean_l * ( alpha + beta/age + gamma * age)
    list(alpha = alpha,
         beta = beta,
         gamma = gamma,
         mean_l)
  )
}

