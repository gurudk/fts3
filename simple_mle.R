
gauss_func <- function(x, mu, sigma){
  value <- -((x-mu)^2)/(2*(sigma^2)) - log(sqrt(2*pi*sigma^2))
  return(value)
}

ll_func_wrapper <- function(data_sample, start){
  result <- optim(start, fn <- function(par){
    mu <- par[1]
    sigma <- par[2]
    
    res <- 1
    for(x in data_sample){
      res <- res + gauss_func(x, mu, sigma)
    }
    
    return(-res)
  })
  
  return(result)
}

ll_func_wrapper_dnorm <- function(data_sample, start){
  result <- optim(start, fn = function(par){
    mu <- par[1]
    sigma <- par[2]
    
    prob <- dnorm(data_sample, mu, sigma)
    
    return(-sum(log(prob)))
  })
  
  return(result)
}

test_multi_func <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  
  return ((x1-1)^2 + (x2-1)^2)
}

