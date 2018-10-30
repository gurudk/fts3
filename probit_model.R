#
#  probit model estimation and simulation 
#
# 1. gen sample data
# 2. object function
# 3. estimate beta
# 4. calculate variance

probit_model <- function(data_sample){
  x <- data_sample$x
  y <- data_sample$y
  rows = dim(x)[1]
  
  result <- optim(c(1,1,1), fn <- function(beta){
    ll_value = 0
    for(i in c(1:rows)){
      ll_value <- ll_value + y[i]*log(pnorm(x[i,] %*% beta,0,1)) + 
        (1-y[i])*(log(1-pnorm(x[i,] %*% beta,0,1)))
    }
    
    return(-ll_value)    
  })
  
  return(result)
}

#
# Generate data for probit model
# @beta0 true value for beta
# @n count of samples
#
probit_data_gen <- function(beta0, n){
  x <- matrix(0, nrow = n, ncol = length(beta0))
  y <- matrix(0, nrow = n, ncol =1)
  for(i in c(1:n)){
    x[i,] <- runif(length(beta0), 0, 1)
    y_star <- x[i,] %*% as.matrix(beta0) + rnorm(1, 0, 1)
    if(y_star > 0){
      y[i] = 1
    }
  }
  
  return(list("x" = x, "y" = y))
}

# estimation variance
variance <- function(estimation_result, data_sample){
  x <- data_sample$x
  y <- data_sample$y
  
  rows <- dim(data_sample$x)[1]
  beta_est <- as.matrix(estimation_result$par)
  v <- matrix(0,nrow = length(beta_est), ncol = length(beta_est))
  for(i in c(1:rows)){
    xbeta <- x[i,] %*% beta_est
    v <- v + as.numeric((dnorm(xbeta)^2/(pnorm(xbeta)*(1-pnorm(xbeta)))))
      * (x[i,] %*% t(x[i,]))
  }
  
  return(v/rows)
}
