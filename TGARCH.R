#
# TGARCH Model 
#
# Model leverage effect
#
# this simulation case is TGARCH(1,1)
# r(t) = phi0 + phi1 * r(t-1) + a(t)
# a(t) = sigma(t) * epsilon(t) 
# sigma(t)^2 = alpha0 + (alpha1 + gamma1 * N(t-1))*(sigma(t-1) * epsilon(t-1))^2 + beta1*sigma(t-1)^2
# if a(t-1) < 0  then N(t-1) = 1 else N(t-1) = 0 
#
# @Restrictions: 
#    alhpa1 >= 0, beta1 <=1, alpha1 + beta1 <1
# 
#
# Data generation order:
#   epsilon(t-1) -> sigma(t)  -> a(t)  -> r(t)
#               |            |        |
#    sigma(t-1)-  epsilon(t)-  r(t-1)-


alpha0 <- 1
alpha1 <- 0.5
beta1 <- 0.2
gamma1 <- 0.1

tgarch11 <- function(N, phi1){
  sigma <- array(0, N)
  epsilon <- rnorm(N, 0, 1)
  a <- array(0, N)
  r <- array(0, N)
  
  for(i in c(1:N)){
    if(i == 1){
      epsilon_0 <- rnorm(1,0,1)
      a_0 <- epsilon_0
      sigma_0 <- 1
      if(a_0 < 0){
        sigma[1] <- sqrt(alpha0 + alpha1 * (1 + gamma1) * (sigma_0 * epsilon_0)^2 + beta1 * sigma_0^2)
      }else{
        sigma[1] <- sqrt(alpha0 + alpha1 * (sigma_0 * epsilon_0)^2 + beta1 * sigma_0^2)
      }
      
      a[1] <- sigma[1] * epsilon[1]
      
      #this round is ok
      next
    }
    
    if(a[i-1] < 0){
      sigma[i] <- sqrt(alpha0 + alpha1 * (1 + gamma1) * (sigma[i-1] * epsilon[i-1])^2 + beta1 * sigma[i-1]^2)
    }else{
      sigma[i] <- sqrt(alpha0 + alpha1 * (sigma[i-1] * epsilon[i-1])^2 + beta1 * sigma[i-1]^2)
    }
    
    a[i] <- sigma[i] * epsilon[i]
    r[i] <- phi1 * (if(i == 1) 0 else r[i-1]) + a[i]
  }
  
  return(r)
}