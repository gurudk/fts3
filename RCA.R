#
# RCA model
#
# random coefficient autoregressive model
#
# r(t) = phi0 + (phi1 + delta1) * r(t-1) + (phi2 + delta2) * r(t-2) + a(t)
# 

rca2 <- function(N, phi){
  r <- array(0, N)
  a <- rnorm(N, 0, 1)
  
  for(i in c(1:N)){
    delta <- runif(2, -1, 1)
    
    if(i == 1){
      r[i] <- a[i]
      next
    }else if(i == 2){
      r[2] <- (phi[1] + delta[1]) * r[i-1] + a[i]
    }else{
      r[i] <- (phi[1] + delta[1]) * r[i-1] + (phi[2] + delta[2]) * r[i-2] + a[i]
    }
  }
  
  return(r)
}