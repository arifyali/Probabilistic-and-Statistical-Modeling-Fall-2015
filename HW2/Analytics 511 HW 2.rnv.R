####Problem 9
r = 2.5
p = 5
###P<=10
pgamma(q = 10, shape = r, scale = p)
pgamma(q = 5, shape = r, scale = p, lower.tail = F)
#P(abs(X-8)<3)=P(X<11) & P(X>5) = P(5<X<11)
pgamma(q = 5, shape = r, scale = p, lower.tail = F) - pgamma(q = 11, shape = r, scale = p, lower.tail = F)
i = 4
z = pgamma(q = i, shape = r, scale = p)
while(z<.1){
i = i + .001  
z = pgamma(q = i, shape = r, scale = p)   
} 
i
####Problem 10
x = 0:20
plot(x, pbinom(x, 20, prob = 1/3), type = 's')
lines(x, phyper(x, m = 40, n = 80, k = 20), type = 's', col = "red")
####Problem 11
x = 0:20
plot(x, pbinom(x, 40, prob = .3), type = 's')
lines(x, pnorm(x, mean = 12, sd = 2.9), type = 's', col = "red")
####Problem 12
# par(mfrow=c(3, 2))
# sizes = c(10, 20, 40, 100, 1000)
# for(i in sizes){
#   qqnorm(rnorm(i), title = paste(c("sample size", as.character(i))))
# }

#rnorm with a size of 10 does not resemble anything like a line
#rnorm with a size of 20 does not resemble anything like a line,
#but shows some aspects of straigthening out in the middle
#rnorm with a size of 40 is straigthening out in the middle
#rnorm with a size of 100 is straigthening out in the middle a little more
#rnorm with a size of 1000 is straigthening out in the middle, but not at the tail
####Problem 13

par(mfrow=c(3, 2))
sizes = seq(from = 20, to = 1000, by = 245)
for(i in sizes){
  
  qqnorm(dbinom(1:i, 40, prob = .3), main = paste(c("sample size", as.character(i))))
}
####Problem 14
###A
pnorm(3, lower.tail = F)
###B
pnorm(42, mean = 35, sd = 6, lower.tail = F)
###C
dbinom(10, 10, prob = 0.8)
###D
punif(0.9) 
###E
pchisq(6.5, df = 2, lower.tail = F)
####Problem 15
par(mfrow=c(3, 1))

x = rnorm(1000)
Fx = pnorm(x)
plot(ecdf(Fx))

x  = rbeta(1000, shape1 = 1, shape2 = 1)
Fx = rbeta(x, shape1 = 1, shape2 = 1)
plot(ecdf(Fx))

x = rgamma(1000, shape = 2, scale = 1)
Fx = pgamma(q = x, shape = 2, scale = 1)
plot(ecdf(Fx))
####Problem 16
#par(mfrow=c(2, 5))
X1 = rexp(1:1000,1)
X2 = rexp(1:1000,1)
X = X1+ X2
A = seq(from = .1, to = 1, by = .1)
# for(i in A){
#   qqnorm(X^i, main = paste("a is", i))
# }
# From this simulation we can infer a is between .2 and .3,
# but closer to .3


