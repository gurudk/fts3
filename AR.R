#
# time series AR(1) simulation
# r(t) = phi0 + phi1*r(t-1) + a(t)
#

ar1 <- function(N, phi0, phi1){
  r <- array(0,N)
  ep <- rnorm(N, 0, 1)
  r[1] <- phi0 + ep[1]
  for(i in c(2:N)){
    r[i] <- phi0 + phi1 * r[i-1] + ep[i]
  }
  
  return(r)
}

#
# ar(p)
#
ar <- function(coefs, N){
  nc <- length(coefs)
  r <- array(0, N)
  ep <- rnorm(N, 0, 1)
  p <- nc - 1
  for(i in c(1:N)){
    r[i] <- coefs[1] + ep[i] #phi0, epsilon
    for(j in c(2:nc)){
      if((i-j+1) <= 0){
        next
      }
      r[i] <- r[i] + r[i-j+1] * coefs[j]
    }
  }
  return(r)
}