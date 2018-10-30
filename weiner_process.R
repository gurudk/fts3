#
# Weiner process 
#
# https://en.wikipedia.org/wiki/Wiener_process
# 

weiner_process <-function(T, n){
  delta = T/n
  W <- array(0,n)
  for(t in c(2:n)){
    W[t] <- W[t-1] + rnorm(1, 0, sqrt(delta))
  }
  
  return(W)
}

draw_weiner <- function(T, n, times){
  data_ <- matrix(ncol = n, nrow = times)
  for(i in c(1:times)){
    data_[i,] <- weiner_process(T,n)
  }
  
  plot(data_[1,], col = rgb(runif(1,0,1), runif(1,0,1), runif(1,0,1)), type='l', ylim = c(min(data_), max(data_)))
  for(i in c(2:times)){
    lines(data_[i,], col = rgb(runif(1,0,1), runif(1,0,1), runif(1,0,1)));
  }
}