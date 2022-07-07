g3a_initial_vonb <- function(Linf, K, recl, K_scale = 0.001){
  f_substitute(
    ~Linf * (1 - exp(-1 * (K_scale * K) * (age - (1 + log(1 - recl/Linf)/(K_scale*K))))),
    list(K_scale = K_scale,
         Linf = Linf,
         K = K,
         recl = recl))
  
}
