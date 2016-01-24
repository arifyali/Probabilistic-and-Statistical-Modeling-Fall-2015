######### R Diary of 10/19

# Prepare to plot records of samples from an exponential distribution:

x <- rexp(40000)
y <- sapply(1:40000, function(n){max(x[1:n])})

# The entry y[k]  now contains the maximum of x[1:k].

plot(y)

# Same thing with a sample from a Cauchy distribution:

x <- rcauchy(40000)
y <- sapply(1:40000, function(n){max(x[1:n])})
plot(y)

# Typical waiting times between jumps (i.e., new records) become larger and larger. 
# For the exponential distribution, all jumps are approximately of the same size. 
# For the Cauchy distribution, each new record is some kind of multiple of the previous one.

# Illustration of the law of large numbers

# Set up to make  N  simulations of sample means, each from a sample of size  n. 

N = 1000 # number of means
n = 10  # sample size in each case
x <- replicate(N, mean(rexp(n)))
boxplot(x)

# Repeat with a larger sample size.

N = 1000 # number of means
n = 100  # sample size in each case
x <- replicate(N, mean(rexp(n)))
boxplot(x)

# Repeat with an even larger sample size

N = 1000 # number of means
n = 1000  # sample size in each case
x <- replicate(N, mean(rexp(n)))
boxplot(x)

# The box plots show that the means become more and more 
# closely concentrated about the true mean, which is  1.

# Plot running from a sample.
# This is done with the function myllnplot() (available on Blackboard).

x <- rexp(10000)
y <- myllnplot(x,1,0)
x <- rexp(10000)
y <- myllnplot(x,1,0)

# These simulations illustrate again that sample means converge to the true mean.

# Now multiply the running means by sample size raised to the power alpha = 1 and plot.
# This now looks like a random walk. 

> y <- myllnplot(x,1,alpha = 1)

# Running means by sample size raised to the power alpha = .2.
# This is a very low power, and the result still converges to zero. 

# Increase the power alpha and observe what happens.

y <- myllnplot(x,1,.2)
y <- myllnplot(x,1,.4)
y <- myllnplot(x,1,.5)

# It turns out that this is the right power for  y  to have a nontrivial behavior 
# The values don't converge to zero and don't go to infinity.

# Illustration of the Central Limit Theorem.
# Make  N  means, each from a sample of size  n. 

# Case: exponential distribution.

N = 1000 # number of means
n = 10  # sample size in each case

qqnorm(x)
hist(x,breaks = 30)

# The quantile – quantile plot and the histogram show that 
# sample means of sample size  n = 10 aren't distributed normally yet.
# The reason is that the original distribution is strongly skewed.
# Therefore, sample means for sample size 10 are also somewhat skewed.

N = 1000 # number of means
n = 100  # sample size in each case
x <- replicate(N, mean(rexp(n)))
qqnorm(x)
hist(x,breaks = 30)

# If the sample size is n =100, even a strongly skewed distributions such 
# as the exponential one have sample means which are approximately normal.

# Case: Uniform distribution

N = 1000 # number of means
n = 10  # sample size in each case
x <- replicate(N, mean(runif(n)))
qqnorm(x)

# For the uniform distribution (symmetric, a single peak),
# sample means for sample size  n = 10 are already approximately normally distributed.

# Is there something like a Central Limit Theorem 
# for standard deviations?

# Follow the same process as before:
# make many samples of a suitable size, 
# compute their standard deviations, and examine the distribution.

N = 1000 # number of sd's
n = 10  # sample size in each case
x <- replicate(N, sd(rexp(n)))
qqnorm(x)
hist(x,breaks = 30)

# Not normally distributed. Increase the sample size to n = 30:

N = 1000 # number of sd's
n = 30  # sample size in each case
x <- replicate(N, sd(rexp(n)))
qqnorm(x)
hist(x,breaks = 30)

# Increase the sample size to n = 100:

n = 100  # sample size in each case
x <- replicate(N, sd(rexp(n)))
qqnorm(x)
hist(x,breaks = 30)

# This is now approximately normally distributed.

# Standard deviations for samples from a uniform distribution:

N = 1000 # number of sd's
n = 10  # sample size in each case
x <- replicate(N, sd(runif(n)))
qqnorm(x)
hist(x,breaks = 30)

# Yes, this looks approximately normal.
# How about a very small sample size?

N = 1000 # number of sd's
n = 2  # sample size in each case
x <- replicate(N, sd(runif(n)))
qqnorm(x)
hist(x,breaks = 30)

# Definitely not normally distributed!

# Are maxima of large random samples normally distributed?
# Try this for samples of size n = 100 from exponential distributions.

n = 100
N = 1000
z <- replicate(N, max(rexp(n,3)))
plot.ecdf(z)
qqnorm(z)

# This is definitely not a normal distribution.
# How to plot the ecdf's from several samples in the same plot:

z <- replicate(N, max(rt(n,df = 3)))
plot.ecdf(z,xlim = c(0,50), lwd = 3) # This is the ecdf of the first sample

# Make another sample and make its empirical cdf:

z <- replicate(N, max(rt(n,df = 3)))
ecdf1 = ecdf(z) # This is now an R function and we can plot it:

x = seq(0,50,by=.1)
lines(x,ecdf1(x), col = 2, lwd = 3) # Plot this one in red.

# Make another sample, and plot the ecdf of that one also.

z <- replicate(N, max(rt(n,df = 3)))
ecdf1 = ecdf(z)
lines(x,ecdf1(x), col = 3, lwd = 3) # Plot this one in green.

######### End of R diary of 10/19