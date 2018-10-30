#
# ARMA(1,1) model
# r(t) = phi0 + phi1 * r(t-1) + a(t) - theta1 * a(t-1)
#
#

arma11 <- function(N, phi0, phi1, theta1){
  a <- rnorm(N, 0, 1)
  r <- array(0, N)
  for(i in c(1:N)){
    r[i] = phi0 + phi1 * (if(i==1) 0 else r[i-1]) + 
      a[i] - theta1 * (if(i==1) 0 else a[i-1])
  }
  
  return(r)
}