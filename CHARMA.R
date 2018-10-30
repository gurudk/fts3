#
# CHARMA model
#
# Conditional heteroskedasticity ARMA model
#
# r(t) = phi0 + phi1 * r(t-1) + a(t)
# a(t) = delta1(t) * a(t-1) + delta2(t) * a(t-2) + yita(t)
# 

charma12 <- function(N, phi1){
  r <- array(0, N)
  a <- array(0, N)
  yita <- rnorm(N, 0, 1)
  
  for(i in c(1:N)){
    delta <- runif(2, 0, 1)
    
    if(i == 1){
      a[i] <- yita[i]
      r[i] <- a[i]
      next
    }else if(i == 2){
      a[i] <- delta[1] * a[i-1] + yita[i]
    }else{
      a[i] <- delta[1] * a[i-1] + delta[2] * a[i-2] + yita[i]
    }
    
    r[i] <- phi1 * r[i-1] + a[i]
  }
  
  return(r)
}