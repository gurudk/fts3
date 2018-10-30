#
# GARCH-M Model 
#
# The GARCH-in-mean (GARCH-M) model adds a heteroskedasticity term 
# into the mean equation 
#
# this simulation case is GARCH(1,1)-M
# r(t) = mu + c * sigma(t)^2 + a(t)
# a(t) = sigma(t) * epsilon(t) 
# sigma(t)^2 = alpha0 + alpha1 * epsilon(t-1)^2 + beta1 * sigma(t-1)^2
# 
# @Restrictions: 
#    alhpa1 >= 0, beta1 <=1, alpha1 + beta1 <1
# 
# Data generation order:
#   epsilon(t-1) -> sigma(t)  ->   a(t)  -> r(t)
#               |            |          |
#    sigma(t-1)-  epsilon(t)-  sigma(t)-


alpha0 <- 1
alpha1 <- 0.5
beta1 <- 0.2
c <- 0.1
mu <- 0.1
garch11_m <- function(N, phi1){
  sigma <- array(0, N)
  epsilon <- rnorm(N, 0, 1)
  a <- array(0, N)
  r <- array(0, N)
  
  for(i in c(1:N)){
    sigma[i] <- sqrt(alpha0 + alpha1 * (if(i==1) rnorm(1,0,1) else epsilon[i-1])^2
                     + beta1 * (if(i==1) 0 else sigma[i-1])^2)
    a[i] <- sigma[i] * epsilon[i]
    r[i] <- mu + c * sigma[i]^2 + a[i]
  }
  
  return(r)
}