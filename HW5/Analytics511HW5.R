#####Exercise 33
par(mfrow = c(2,2))
Xa = runif(1000, 0, 60)
Xb = runif(1000, 0, 60)
Wa = (Xb-Xa)
Wa[Wa<0] = 0
mean(Wa)
plot.ecdf(Wa, main = "Exercise 33")

#####Exercise 34
x = c()
y = c()
for(i in 1:10000){
x[i] = rexp(1,1)
y[i] = rpois(1,x[i])}
length(x[y == 2])
hist(x[y == 2], breaks = 20, main = "Exercise 34")
#####Exercise 36
#from ANALY511functions
mybivariate = function(n, mx = 0, my = 0,  sigx = 1, sigy = 1, rho = 1/2){
  Z1 <- rnorm(n)
  Z2 <- rnorm(n)
  X <- sigx*Z1 + mx
  Y <- rho*sigy*Z1 + sqrt(1-rho^2)*sigy*Z2
  z <- matrix(c(X,Y),ncol = 2)
  #plot(z[,1],z[,2],asp = 1)
  #grid(col = 1)
  return(z)
}
Exercise36 = mybivariate(10000)
cor(Exercise36[,1], Exercise36[,2])
Z= Exercise36[,2][Exercise36[,1]>=1]
mean(Z)
VarZ = mean(Z^2)-(mean(Z))
sqrt(VarZ)
sd(Z)
qqnorm(Z,main = "Exercise 36")


par(mfrow = c(2,1))
#####Exercise 39
x = c()
y = c()
for(i in 1:10000){
  x[i] = rexp(1,1)
  y[i] = rpois(1,x[i])}
hist(y , breaks = 20,main = "Exercise 39 Y")
hist(x[y == 2], breaks = 20,main = "Exercise 39 X|Y=2")

#####Exercise 40
###Part 1
w1 = 1/3
w2 = 2/3

Y = rep(0, times = 1000)

X = sample(c(1,2),1000,  prob = c(w1, w2), replace = T)
Y[X == 1] = rnorm(length(Y[X == 1]), -1, 1)
Y[X == 2] = rnorm(length(Y[X == 2]), 2, 2)


hist(Y, breaks = 20,main = "Exercise 40")

