#
# Autoregressive conditional heteroskedasticity(ARCH)

# Simulate ARCH(1)
# r(t) = phi0 + phi1 * r(t-1) + a(t)
# a(t) = sigma(t) * epsilon(t)
# sigma(t)^2 = alpha0 + alpha1 * epsilon(t-1)^2 
# Restrictions : 0<= alpha1 <1 and 0<= alpha1 < 1/sqrt(3)
# a(t) is stationary, alpha0 not equal zero
# alpha0 = 1 and alpha1 = 0.5 in this simulation

# @reference https://en.wikipedia.org/wiki/Autoregressive_conditional_heteroskedasticity
# @return ARCH(1) time series samples

alpha0 <- 1
alpha1 <- 0.5
arch1 <- function(N, phi1){
  sigma <- array(0, N)
  epsilon <- rnorm(N, 0, 1)
  a <- array(0, N)
  r <- array(0, N)
  for(i in c(1:N)){
    sigma[i] <- sqrt(alpha0 + alpha1 * (if(i==1) rnorm(1,0,1) else epsilon[i-1])^2)
    a[i] <- sigma[i] * epsilon[i]
    r[i] <- phi1 * (if(i == 1) 0 else r[i-1]) + a[i]
  }
  
  return(r)
}
