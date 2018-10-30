#
# moving average model, MA1 simulation
# r(t) = c0 + a(t)-theta1 * a(t-1)
#

ma1 <- function(N, c0, theta1){
  a <- rnorm(N, 0, 1)
  r <- array(0, N)
  for(i in (1:N)){
    r[i] <- c0 + a[i] - theta1 * (if(i == 1) 0 else a[i-1])
  }
  
  return(r)
}