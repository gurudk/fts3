#
# Multivariate time series 
#
#
gamma_0 <- function(){
  data_ <- read.table("./data/m-mrk2vw.txt", header = T)[,2:7]
  mu <- colMeans(data_)
  x_mu <- t(data_) - mu
  gamma_0 <- (x_mu %*% t(x_mu)) / dim(data_)[1]

  return(gamma_0)
}

rho_0 <- function(){
  data_ <- read.table("./data/m-mrk2vw.txt", header = T)[,2:7]
  mu <- colMeans(data_)
  x_mu <- t(data_) - mu
  gamma_0 <- (x_mu %*% t(x_mu)) / dim(data_)[1]
  D <- sqrt(diag(diag(gamma_0)))
  D_INV <- solve(D)
  rho_0 <- D_INV %*% gamma_0 %*% D_INV
  
  
  return(rho_0)
}

gamma_l <- function(l){
  data <- read.table("./data/m-mrk2vw.txt", header = T)[,2:7]
  
  mu <- colMeans(data)
  T <- dim(data)[1]
  x_mu_l_T <- (t(data) - mu)[,-1:-l]
  x_mu_1_T_l <- (data - mu)[-(T-l+1):-T,]
  gamma_l <- (x_mu_l_T %*% as.matrix(x_mu_1_T_l))/T
  return(gamma_l)
}

rho_l <- function(gamma_l, gamma_0){
  D <- sqrt(diag(diag(gamma_0)))
  D_INV <- solve(D)
  rho_l <- D_INV %*% gamma_l %*% D_INV
  
  return(rho_l)
}

Q <- function(gamma_0, m, T){
  tr_sum <- 0
  for(i in c(1:m)){
    tr_sum <- tr_sum + (1/(T-i))*sum(diag(t(gamma_l(i)) %*% solve(gamma_0) %*% gamma_l(i) %*% solve(gamma_0)))  
  }
  
  Q <- T^2 * tr_sum
  return(Q)
}

# H0: ρ1=ρ2=ρ3=ρ4=ρ5=ρ6=0 
test_Ljung_Box <- function(){
  g0 <- gamma_0()
  
}

