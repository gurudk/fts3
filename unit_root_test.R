
source("UNITROOT.R")

no_constant_test_statistic <- function(T, times){
  result <- array(0, times)
  for(i in c(1:times)){
    # generate random walk series
    y <- random_walk(T)
    y_1 <- c(0,y[-T])
    
    # get errors
    u <- c(y[1],(y[-1] - y[-T]))
    
    #compute no constant test statistic
    ncts <- T*((sum(y_1 * u))/sum(y_1^2))
    result[i] <- ncts
  }
  
  return(result)
}

draw_hist_no_constant <-function(T, times){
  hist(no_constant_test_statistic(T,times), col = 'blue', breaks = 100)
}

dickey_fuller_test <- function(T, times){
  t_test_results <- array(0, times)
  for(i in c(1:times)){
    # generate random walk series
    y <- random_walk(T)
    y_1 <- c(0,y[-T])
    
    # get errors
    u <- c(y[1],(y[-1] - y[-T]))
    
    #compute dickey fuller test
    rho_T <- (sum(y * y_1))/sum(y_1^2)
    sd_T <- sqrt(sum((y-rho_T * y_1)^2)/(T - 1))
    dickey_fuller_stat <- (rho_T-1)/(sd_T/sqrt(sum(y[-T]^2)))
    t_test_results[i] <- dickey_fuller_stat
  }
  
  return(t_test_results)
}

draw_dicky_fuller <- function(T, times, breaks){
  dickey_data <- dickey_fuller_test(T, times)
  hist(dickey_data, col = 'blue', breaks)
}

with_constant_test <- function(T, times){
  results <- array(0, times)
  for(i in c(1:times)){
    # generate random walk series
    y <- random_walk(T)
    y_1 <- c(0,y[-T])
    
    # get errors
    u <- c(y[1],(y[-1] - y[-T]))
    
    i_W <- T^(-3/2)*sum(y_1)
    i_W_square <- T^(-2)*sum(y_1^2)
    i_W_dW <- T^(-1)*sum(y_1 * u)
    # W(1) ~ normal(0, 1)
    W_1 <- u[T]
    
    #compute test statistic with constant
    stat <- ((W_1^2 - 1)/2 - W_1 * i_W) / (i_W_square - i_W^2)
    results[i] <- stat
  }
  
  return(results)
}


with_constant_t_test <- function(T, times){
  results <- array(0, times)
  for(i in c(1:times)){
    # generate random walk series
    y <- random_walk(T)
    y_1 <- c(0,y[-T])
    
    # get errors
    u <- c(y[1],(y[-1] - y[-T]))
    
    i_W <- T^(-3/2)*sum(y_1)
    i_W_square <- T^(-2)*sum(y_1^2)
    i_W_dW <- T^(-1)*sum(y_1 * u)
    # W(1) ~ normal(0, 1)
    W_1 <- u[T]
    
    #compute test statistic with constant
    stat <- ((W_1^2 - 1)/2 - W_1 * i_W) / sqrt((i_W_square - i_W^2))
    results[i] <- stat
  }
  
  return(results)
}


draw_with_constant <- function(T, times, breaks){
  data_ <- with_constant_test(T, times)
  hist(data_, col = 'blue', breaks)
}


draw_t_test_with_constant <- function(T, times, breaks){
  data_ <- with_constant_t_test(T, times)
  hist(data_, col = 'blue', breaks)
}