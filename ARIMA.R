#
# time series ARIMA(1,1,0) simulation
# dr(t) = phi0 + phi1*dr(t-1) + a(t)
# dr(t) = r(t) - r(t-1)
# that means dr(t) is AR(1) process
#

arima110 <- function(N, phi0, phi1){
  dr <- array(0,N)
  dr[1] <- phi0 + rnorm(1, 0, 1)
  for(i in c(2:N)){
    dr[i] <- phi0 + phi1 * dr[i-1] + rnorm(1, 0, 1)
  }
  
  r <- array(0, N)
  
  #integrated
  for(i in (1:N)){
    if(i == 1){
      r[i] <- 0
    }else{
      r[i] <- sum(dr[1:i])
    }
  }
    
  return(list("dr" = dr, "r" = r))
}