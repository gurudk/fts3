
polya_1 <- function(){
  jar <- c(0,1)
  indicator <- 3
  round_percent <- rep(0,2000)
  for(i in c(1:2000)){
    jar <- c(0,1)
    for(j in c(1:998)){
      idx <- ceiling(runif(1,0,length(jar)))
      jar <- c(jar, jar[idx])
    }
    round_percent[i] <- sum(jar)/1000
  }
  return(round_percent)
}

polya_2 <- function(sim_rounds, ast){
  jar <- c(0,1)
  res <- array(dim = c(sim_rounds, ast))
  
  for(i in c(1:sim_rounds)){
    jar <- c(0,1)
    
    a = 1
    total <- 1000 * ast
    for(j in c(3:total)){
      idx <- ceiling(runif(1,0,length(jar)))
      jar <- c(jar, jar[idx])
      
      if((j %% 1000 ) == 0){
        res[i,a] <- sum(jar)/j
        a <- a + 1
      }
    }
  }
  
  return(res)
}