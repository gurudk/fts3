#
# some base statistics
#
#

autorelation <- function(ts, lag){
  T <- length(ts)
  mu <- mean(ts)
  
  ar <- (sum((ts[-1:-lag] - mu) * (ts[-T:-(T- lag + 1 )] - mu))/sum((ts - mu)^2))
  
  return(ar)
}

get_autorelations <- function(ts, lags){
  result <- array(0, length(lags))
  for(i in lags){
    result[i] <- autorelation(ts, i)
  }
  
  return(result)
}

t_radio <- function(ts, lag){
  rho_lag <- autorelation(ts, lag)
  T <- length(ts)
  if(lag == 1){
    t_denominator <- sqrt(1/T)
  }else{
    t_denominator <- sqrt(1+2*(sum(get_autorelations(ts, c(1:(lag-1)))^2)/T))
  }
  
  t_radio <- rho_lag / t_denominator
  return(t_radio)
}

test_autorelation <- function(ts, lag){
  return(get_autorelations(ts, c(1:lag)))
}

box_pierce_test_statistic <- function(ts, m){
  T <- length(ts)
  Q <- T * sum(get_autorelations(ts, c(1:m))^2)
  return(Q)
}

Ljung_Box_test_statistic <- function(ts, m){
  Q <- 0
  T <- length(ts)
  for(i in c(1:m)){
    Q <- Q + (autorelation(ts, i)^2)/(T-i)
  }
  Q <- T * (T + 2) * Q
  return(Q)
}

#
# Partial autoregression coefficients
#
pacf <- function(ts ,orders){
  T <- length(ts)
  pacf <- array(0, orders)
  for(i in c(1:orders)){
    #for every order
    # 1. prepare regressoin data
    # 2. run regression
    # 3. get result coefficients
    y <- ts[-1:-i]
    x <- matrix(0, ncol = i, nrow = (T - i))
    for(j in c(1:i)){
      x[,j] <-  ts[(i - j + 1):(T-j)]
    }
    
    lr <- lm(y ~ x[,c(1:i)])
    # get the last factor coefficient
    pacf[i] <- lr$coefficients[length(lr$coefficients)]
  }
  
  return(pacf)
}

ar.aic <- function(ts, lag){
  # 1. run lag l regrsssion
  # 2. estimate coefficients
  # 3. calculate fitted value
  # 4. calculate residual
  # 5. estimate variance
  # 6. calculate aic
  T <- length(ts)
  y <- ts[-1:-lag]
  x <- matrix(0, ncol = lag, nrow = (T - lag))
  for(j in c(1:lag)){
    x[,j] <-  ts[(lag - j + 1):(T-j)]
  }
  
  lr <- lm(y ~ x[,c(1:lag)])
  a <- ts[-1:-lag] - lr$fitted.values
  sigma_square <- sum(a^2)/(T - lag) 
  
  aic <- log(sigma_square) + 2*lag/T
  
  return(aic)
}

ar.aics <- function(ts, tolag){
  aics <- array(0, tolag)
  for(i in c(1:tolag)){
    aics[i] <- ar.aic(ts, i)
  }
  
  return(aics)
}

ar.bic <- function(ts, lag){
  # 1. run lag l regrsssion
  # 2. estimate coefficients
  # 3. calculate fitted value
  # 4. calculate residual
  # 5. estimate variance
  # 6. calculate bic
  T <- length(ts)
  y <- ts[-1:-lag]
  x <- matrix(0, ncol = lag, nrow = (T - lag))
  for(j in c(1:lag)){
    x[,j] <-  ts[(lag - j + 1):(T-j)]
  }
  
  lr <- lm(y ~ x[,c(1:lag)])
  a <- ts[-1:-lag] - lr$fitted.values
  sigma_square <- sum(a^2)/(T - lag) 
  
  bic <- log(sigma_square) + 2*log(T)/T
  
  return(bic)
}

ar.bics <- function(ts, tolag){
  bics <- array(0, tolag)
  for(i in c(1:tolag)){
    bics[i] <- ar.bic(ts, i)
  }
  
  return(bics)
}







