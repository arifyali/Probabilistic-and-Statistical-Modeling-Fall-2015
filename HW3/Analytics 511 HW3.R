#####Problem 17
X = rbeta(1000, .5, .8)
EX = mean(X)
sdX = sqrt(mean(X^2)-EX^2)
sd(X) #Note: built sd factors in (N-1)/N coeffient
Ex_.333 = mean(X^(-1/3))
#####Problem 18
#####Problem 19
X = runif(10000)
Y = X[(X<=(sin(2*pi*X))^2)]

