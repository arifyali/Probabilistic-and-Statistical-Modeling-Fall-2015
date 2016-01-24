# R functions for September 2, 2015

# Simulate single toos, success probability  p 

mytoss = function(p){ 
  u <- runif(1) 
  x <- as.numeric(u < p)
  return(x) 
  }

# Simulate number of attempts until first success, success probability  p 

myattempts = function(p){ 
  counter <- 1 
  while (mytoss(p) == 0){
    counter <- counter + 1} 
  return(counter) 
}

# R functions for October 5, 2015

# Simulate and plot bivariate normal distribution

mybivariate = function(n, mx = 0, my = 0,  sigx = 1, sigy = 1, rho = 0){
  Z1 <- rnorm(n)
  Z2 <- rnorm(n)
  X <- sigx*Z1 + mx
  Y <- rho*sigy*Z1 + sqrt(1-rho^2)*sigy*Z2
  z <- matrix(c(X,Y),ncol = 2)
  plot(z[,1],z[,2],asp = 1)
  grid(col = 1)
  return(z)
}
