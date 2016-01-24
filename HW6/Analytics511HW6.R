#' ---
#' title: "Analytics 511 Homework 6"
#' author: "Arif Ali"
#' date: "Oct 29th, 2015"
#' ---
######Problem 41
Z = (51-48)/(9/sqrt(30))
pnorm(Z, lower.tail = F)
#[1] 0.03394458
######Problem 42
# For each Xi ~ U(0,1) the mean is .5 so the mean of Z is 12*.5-6 = 0
# For the variance of each Xi, it is 1/12*(1-0) = 1/12
# The Variance of Z is 1/12*12 - 0 = 1
# Z has the mean and variance of a standard normal distribution
# By theorem 4,2, since all X are i.i.d witht the same variances and means,
# Then any constant z will follow the normal distribution.
######Problem 43
#Part A
meanY_X = 7-10
#[1] -3
sdY_X = sqrt(9/sqrt(9)+25/sqrt(12))
#[1] 3.196385
#Part B
simX_Y = function(i){
  X = rnorm(9, mean = 7, sd = 3)
  Y = rnorm(12, mean = 10, sd = 5)
  meanY_X = mean(X) - mean(Y)
  sdY_X = sqrt((sd(X))^2/sqrt(length(X))+(sd(Y))^2/sqrt(length(Y)))
  return(list(meanY_X, sdY_X))
}
Problem43B = sapply(1:10000, simX_Y)

Problem43B = as.data.frame(t(Problem43B))
hist(unlist(Problem43B$V1), breaks = 25)
abline(v = meanY_X, col = "red", lwd = 2)
mean(unlist(Problem43B$V1))
hist(unlist(Problem43B$V2), breaks = 25)
abline(v = sdY_X, col = "red", lwd = 2)
mean(unlist(Problem43B$V2))
# Based on the histogram and the means of the simulationed SE and mean,
# both seem close to the theoretical ones.  

######Problem 44
#Part A
X20 = replicate(1000, sum(rexp(20, rate = 2)))
hist(X20, probability = T, breaks = 25)
#Part B
mean(X20)
var(X20)
#Part C
mean(X20<=10)
######Problem 45
my.vars = sapply(rep(20, times = 1000), function(x){var(rnorm(x, 25, 7))})
mean(my.vars)
var(my.vars)
#hist(my.vars, breaks = 25)
# dev.new()
# qqnorm(my.vars)
# qqline(my.vars)
# at n = 20, the qqnorm plot does not follow the the qqline; therefore it
# does not appear to be normally distributed. The Histogram is skrewed
my.vars = sapply(rep(50, times = 1000), function(x){var(rnorm(x, 25, 7))})
mean(my.vars)
var(my.vars)
#hist(my.vars, breaks = 25)
# dev.new()
# qqnorm(my.vars)
# qqline(my.vars)
# at n = 50, the qqnorm plot follows the qqline close than at n = 20, but it
# still deviates a significant amount of the time so, it does not appear to
# be normally distributed. The histogram has two peaks.
my.vars = sapply(rep(200, times = 1000), function(x){var(rnorm(x, 25, 7))})
mean(my.vars)
var(my.vars)
#hist(my.vars, breaks = 25)
# dev.new()
# qqnorm(my.vars)
# qqline(my.vars)
# At n = 200, it closely follows the line so it is normally distributed. The
# histogram is a rough, but looks somewaht bell shaped.
######Problem 46
pop = c(3,6,7,9,11,14)
Problem46 = combn(pop, m = 3, FUN = min)
mean(Problem46)
hist(Problem46)
# It looks like we are trying to estimate the min of the population. That seems
# to be the parameter
######Problem 47
###Part A
#E(X) = 10
###Part B
my.means = replicate(1000, mean(rexp(30, rate = 1/10)))
mean(my.means>=12)
###Part C
#The proportion doesn't seem too small, so it's not unsual.
######Problem 48
###Part A
#  fmin(x) = n(1-(1-e^-(lambda*x)))^(n-1)*lambda*e^(-lambda*x) =
#  n*e^((n-1)*(-lambda*x)*e^-(lambda*x)=n*lambda*e^(-n*lambda*x)
# Xmin ~ exp(n*lambda)
###Part B
Problem48B = replicate(1000, min(rexp(25, rate = 7)))
1/(25*7) - mean(Problem48B)
