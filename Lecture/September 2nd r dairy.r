###############################
#
# R Diary of 09/02
#
# Some output has been deleted.
###############################

# Illustration of round-off behavior of IEEE arithmetic
# The computer represents numbers internally using 64 bits, 
# resulting in about 15 or 16 decimal digits of accuracy. 
# This means that the relative rounding error typically is of the order 10^(-16).

# This can be tested by attracting two numbers which are close to each other.
# Subtract two numbers that differ by 1%. 
# The computation is essentially accurate.

> (100 + 1) - 100
[1] 1

# Subtract two numbers that differ by 1 in 10^20.
# Due to round-off, the two numbers are represented 
# in the same way by the computer. 
# Therefore, the difference is computed as zero, which is incorrect.

> (10^20 + 1) - 10^20
[1] 0

# Subtract two numbers whose relative difference is 10^(-18).

> (10^20 + 100) - 10^20
[1] 0

# Subtract two numbers whose relative difference is 10^(-17).

> (10^20 + 1000) - 10^20
[1] 0

# Subtract two numbers with relative difference 10^(-16).
# Now a nonzero difference is returned, but it is wildly inaccurate.

> (10^20 + 10000) - 10^20
[1] 16384

# Work with functions. Source a script that defines several functions.
# To do this, change the path as needed.

> source('ANLY511functions.r')

# Check if the function "mytoss" works.

> mytoss(.4)
[1] 0
> mytoss(.4)
[1] 0
> mytoss(.4)
[1] 1

# It returns 0s and 1s

# Check if the function "myattempts" works. It should return positive integers.

> myattempts(.4)
[1] 2
> myattempts(.4)
[1] 1
> myattempts(.4)
[1] 3
> myattempts(.4)
[1] 8

# Assess the speed of mytoss.

> system.time(for (j in 1:100000) z <- mytoss(.4))
user  system elapsed 
0.39    0.01    0.41 
> system.time(for (j in 1:100000) z <- mytoss(.4))
user  system elapsed 
0.40    0.00    0.41 
> system.time(for (j in 1:100000) z <- mytoss(.4))
user  system elapsed 
0.41    0.00    0.40 

# About 4 microseconds per function call.
# The speed varies due to other tasks done by the CPU.

# The build an equivalent of "mytoss" is
# 
# rbinom(1,1,p) .
# 
# That is, given any p, repeated calls of mytoss(p) 
# and repeated calls of rbinoom(1,1,p) will give random sequences 
# with essentially the same statistical properties 
# (distribution, independence) 

> system.time(for (j in 1:100000) z <- rbinom(1,1,.4))
user  system elapsed 
0.22    0.00    0.24 

# About twice as fast, 2 microseconds per call.
# Still, this is comparable to mytoss().

# Assess the speed of myattempts().

> system.time(for (j in 1:100000) z <- myattempts(.4))
user  system elapsed 
1.29    0.01    1.32 

# About 13 microseconds per call.
# This is three times as slow as mytoss().
# The reason is that in order to evaluate myattempts(p) 
# for a given p, the function mytoss(p) must be called repeatedly. 

# This is barely noticeable if p is close to 1.

> system.time(for (j in 1:100000) z <- myattempts(.99))
user  system elapsed 
0.59    0.00    0.59 

# About  6 microseconds per call. 
# Since p is close to 1 in this case, the function mytoss() 
# is probably called only once each time myattempts() is evaluated.
# The extra overhead from the while loop leads to a longer execution time 
# than just mytoss().

# On the other hand, if p  is small, many calls to mytoss() 
# are needed to evaluate myattempts() even once, 
# leading to much longer execution time.

> system.time(for (j in 1:100000) z <- myattempts(.1))
user  system elapsed 
4.84    0.00    4.84 

# About 48 microseconds per call, which is very bad.

# Shawna has used 31 attempts to get to 5 successes.
# We don't know what her success probability  p  actually is.
# We wish to determine whether a given hypothesized  p  
# is consistent with the data we have for her.
# To do that, we simulate a large number (e.g. 1000) 
# attempts to reach  5  successes.
# If a large fraction of these attempts get to five successes in
# fewer than 31 tosses, Shauna's success probability
# most likely is less than the  p  that was used for the simulation.
# If a very small fraction of these attempts get to 5 successes in
# 31 attempts or less, then Shauna's success probability is most likely# larger than the  p  that was used in the simulation.

# Prepare a vector that will contain results of the simulation.
# We will simulate 1000 attempts to get to five successes.

> res = rep(0,1000)

# Fix a probability for the simulation 

> p0 = .5

# Carry out the simulation. This requires 5 times 1000 calls to myattempts.().

> for (j in 1:1000){
+ x <- 0
+ for (k in 1:5){
+ x <- x + myattempts(p0)
+ }
+ res[j] <- x
+ }

# Now the vector  res  contains 1000 positive integers.
# Each entry tells us the number of tosses needed 
# to get to five successes for a single simulated player 
# who has  the success probability p0.

# Compute the fraction of entries < 31.

> mean(res < 31)  # Why does this command work?
[1] 1

# All attempts got to five successes in less than 31 tosses.
# Therefore, Shauna's  p  is highly likely to be less than p0 = .5.

# Repeat with a smaller p0.

> p0 = .3 
> for (j in 1:1000){
+   x <- 0
+   for (k in 1:5){
+     x <- x + myattempts(p0)
+   }
+   res[j] <- x
+ }
> mean(res < 31)
[1] 0.976

> Most of the time, a person with success probability p0 = 0.3 
> gets to five successes faster than Shauna.
> Therefore, most likely Shauna's  p  is less than p0 = 0.3. 

# Repeat with a smaller p0.

> p0 = .25

[ Output omitted ]

> mean(res < 31)
[1] 0.908

# Nine out of 10 times, a simulated person with p = 0.25 
# gets to five successes faster than Shauna.
# This is evidence against Shauna having p = .25,
# but it is not as strong as the evidence against Shauna 
# having p = .3.

# Try very small values of p0

> p0 = .1

[output omitted]

> mean(res < 31)
[1] 0.17

# About five out of six times, Shauna gets to five successes
# faster than a person whose success probity is p = .1.
# This is evidence that Shauna's   p  is larger than 0.1, 
# but not very strong. 

# Repeat this a number of times with the same p0. 
# Since we are performing a stochastic simulation each time,
# the results are bit different each time.
# If we had taken a smaller number of simulations,
# the variation between simulations would be larger.

> mean(res < 31)
[1] 0.168
> mean(res < 31)
[1] 0.188
> mean(res < 31)
[1] 0.188
> mean(res < 31)
[1] 0.16
> mean(res < 31)
[1] 0.18

# Assess bias and variation of our plug-in estimate p-hat,
# using the number of tosses to get to  k = 1  successes.
# We pick the success probability p = .3,
# simulate a large number of attempts to get to one success (N = 10000)
# and record all results in a vector.

> p0 = .3
> yvec = rep(0,10000)
> for (j in 1:10000){yvec[j] <- myattempts(p0)}

# Now yvec contains the results of 10,000 simulations.

# Next, compute the plug-in estimate for each simulation.
# Since k = 1, this is simply the reciprocal of each value.

> phat <- 1/yvec

# Compute the mean of all these estimates.

> mean(phat)
[1] 0.5192353

# This differs substantially from the true value  p0 = .3. 
# The bias is defined as the difference:

> mean(phat) - p0
[1] 0.2192353

# For different values of p0, we get a different bias. 

> p0 = .7
> yvec = rep(0,10000)
> for (j in 1:10000){yvec[j] <- myattempts(p0)}
> phat <- 1/yvec
> mean(phat) - p0
[1] 0.1314861

# So for p0 = .7, the bias is approximately 0.13.
# And for p0 = .12, the bias is approximately .16:

> p0 = .12
> yvec = rep(0,10000)
> for (j in 1:10000){yvec[j] <- myattempts(p0)}
> phat <- 1/yvec
> mean(phat) - p0
[1] 0.1632638

# Reconstitute a simulation with p0 = .3.

> p0 = .3
> yvec = rep(0,10000)
> for (j in 1:10000){yvec[j] <- myattempts(p0)}
> phat <- 1/yvec

# Make a box plot. The box plot shows a large spread. The median is about 0.5.

> boxplot(phat)

# Make a histogram. The only possible values of p-hat are 1, 1/2, 1/3, etc., 
# which can be seen in the histogram. 
The largest value is p-hat = 1, which occurs about 30% of the time.

> hist(phat, breaks = 100)

############  End of R diary for 9/2/15  ########################