#
# VAR simulation,for 3 cases:
# case 1:  stationary, |rho_1| < 1, |rho_2| < 1
# case 2: non-stationary but cointegrated, rho_1=1,|rho_2|<1
# case 3: non-stationary, rho_1 = 1 , rho_2 = 1
# 
# @AUTHOR YONGJI ZHANG, SUFE
# @MODEL
#   X(t) + Y(t) = v(t)
#   2*X(t) + Y(t) = u(t)
# there
#   v(t)*(1-rho_1*L) = epsilon_1(t)
#   u(t)*(1-rho_2*L) = epsilon_2(t)

# @GALGORITHM
#   epsilon_1(t)  -> v(t) ----->|
#     v(t-1)    -|              |
#                               |----->X(t) = u(t) - v(t), Y(t) = 2*v(t) - u(t)
#   epsilon_2(t)  -> u(t) ----->|
#     u(t-1)    -|

source("AR.R")

var_model <- function(T, rho_1, rho_2){
  v <- ar1(T, 0, rho_1)
  u <- ar1(T, 0, rho_2)
  
  return(list("X"=(u-v), "Y"=(2*v-u), "v"=v, "u"=u))
}

#
# @T time series length
# @rho_1 AR coeffient for v(t)
# @rho_2 AR coeffient for u(t)
# @times draw times
#
draw_var <- function(T, rho_1, rho_2, times){
  data_ <- array(0, dim = c(times, 4, T))
  for(i in c(1:times)){
    dt <- var_model(T, rho_1, rho_2)
    data_[i,1,] <- dt$X
    data_[i,2,] <- dt$Y
    data_[i,3,] <- dt$v
    data_[i,4,] <- dt$u
  }
  min_ <- min(data_)
  max_ <- max(data_)

  par(xpd = TRUE)
  plot(data_[1,1,], ylab="y", xlab="x", type = 'l', col = 'red', ylim = c(min_, max_ + 10))
  lines(data_[1,2,], col = 'black')
  lines(data_[1,3,], col = "blue")
  lines(data_[1,4,], col = "green")
  
  if(times > 1){  
    for(i in c(2:times)){
      lines(data_[i,1,], col="red")
      lines(data_[i,2,], col = 'black')
      lines(data_[i,3,], col = "blue")
      lines(data_[i,4,], col = "green")
    }
  }
  
  vtext <- as.expression(bquote(paste("v(t), ",rho,"1 = ",.(rho_1))))
  utext <- as.expression(bquote(paste("u(t), ",rho,"2 = ",.(rho_2))))
  legend("topleft", xpd=TRUE, bty = "L", c("X(t)","Y(t)", vtext, utext), col = c("red","black", "blue", "green"), lty=1,lwd=2)
}

 