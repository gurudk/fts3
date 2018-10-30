#
# random walk process
#
# 

random_walk <- function(N){
  a <- rnorm(N, 0, 1)
  p <- array(0, N)
  for(i in c(1:N)){
    if(i == 1){
      p[1] = a[1]
    }else{
      p[i] = p[i-1] + a[i]
    }
  }
  
  return(p)
}
