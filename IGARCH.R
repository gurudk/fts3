#
# IGARCH Model 
# 
# Integrated Generalized Autoregressive Conditional heteroskedasticity 
# (IGARCH) is a restricted version of the GARCH model, 
# where the persistent parameters sum up to one
#
# this simulation case is IGARCH(1,1)
# r(t) = phi0 + phi1 * r(t-1) + a(t)
# a(t) = sigma(t) * epsilon(t) 
# sigma(t)^2 = alpha0 + alpha1 * epsilon(t-1)^2 + beta1 * sigma(t-1)^2
# 
# @Restrictions: 
#    alhpa1 >= 0, beta1 <=1, alpha1 + beta1 = 1
# 
#
# Data generation order:
#   epsilon(t-1) -> sigma(t)  -> a(t)  -> r(t)
#               |            |        |
#    sigma(t-1)-  epsilon(t)-  r(t-1)-


alpha0 <- 1
alpha1 <- 0.5
beta1 <- 0.5
igarch11 <- function(N, phi1){
  sigma <- array(0, N)
  epsilon <- rnorm(N, 0, 1)
  a <- array(0, N)
  r <- array(0, N)
  
  for(i in c(1:N)){
    sigma[i] <- sqrt(alpha0 + alpha1 * (if(i==1) rnorm(1,0,1) else epsilon[i-1])^2
                     + beta1 * (if(i==1) 0 else sigma[i-1])^2)
    a[i] <- sigma[i] * epsilon[i]
    r[i] <- phi1 * (if(i == 1) 0 else r[i-1]) + a[i]
  }
  
  return(r)
}
