summary(mice)
boxplot(time ~ pop, data = mice)

mean(mice$time[mice$pop == "control"])
mean(mice$time[mice$pop == "treatment"])


aggregate(time ~ pop, data = mice, FUN = mean)
tapply(mice$time, mice$pop, FUN = mean)

system.time(for (j in 1:1000){aggregate(time ~ pop, data = mice, FUN = mean) -> dummy})

x0 <- tapply(mice$time, mice$pop, FUN = mean)
x0[1] - x0[2] -> t0
names(t0) <- NULL

perm <- sample(24,24, replace = F)

miceperm <- data.frame(pop = mice$pop, time = mice$time[perm])

boxplot(time ~ pop, data = miceperm)


tapply(miceperm$time, miceperm$pop, mean)

micepermfun = function(){
  perm <- sample(24,24, replace = F)
  miceperm <- data.frame(pop = mice$pop, time = mice$time[perm])
  x <- tapply(miceperm$time, miceperm$pop, mean)
  y <- x[1] - x[2]
  names(y) <- NULL
  return(y)
}

micepermfun()

z <- replicate(50000,micepermfun())

hist(z)
lines(c(t0,t0), c(0,2000), col = 2)
sum(z > t0)

(sum(z > t0) + 1)/(length(z) + 1)

boxplot(Time ~ Group, data = Verizon)

tapply(Verizon$Time, Verizon$Group, mean) -> x1

tapply(Verizon$Time, Verizon$Group, median)

verizonpermfun = function(){
  perm <- sample(1687,1687, replace = F)
  verizonperm <- data.frame(Time = Verizon$Time[perm], Group = Verizon$Group)
  x <- tapply(verizonperm$Time, verizonperm$Group, mean)
  y <- x[1] - x[2]
  names(y) <- NULL
  return(y)
}

z <- replicate(10000, verizonpermfun())

hist(z)

(1+sum(z > t0))/(10001)

# Do this for the median

x2 <- tapply(Verizon$Time, Verizon$Group, median)
t2 <- x2[1] - x2[2]

verizonpermfun1 = function(){
  perm <- sample(1687,1687, replace = F)
  verizonperm <- data.frame(Time = Verizon$Time[perm], Group = Verizon$Group)
  x <- tapply(verizonperm$Time, verizonperm$Group, median)
  y <- x[1] - x[2]
  names(y) <- NULL
  return(y)
}

z1 <- replicate(10000, verizonpermfun1())

hist(z1)

(1+sum(z1 > t2))/(10001)



birth <- c(150,138,140,100)

expected <- rep(mean(birth),4)
X2 = sum((birth - expected)^2/expected)


mysoccerX2 = function(x){return(sum((x - expected)^2/expected))}

sim <- replicate(10000, mysoccerX2(rmultinom(1,528,rep(1/4,4))))

hist(sim)
abline(v = X2, , col = 2, lwd = 2)
mean(sim > X2)

hist(sim, prob = T, breaks = 30)

xx <- seq(0,20,by=.1)
lines(xx,dchisq(xx,3), col = 2, lwd = 2)

pchisq(X2,3, lower.tail=F)


table(GSS2006$Gender)
table(GSS2006$Happy)

table(GSS2006$Gender, GSS2006$Happy) -> MyTable

rowSums(MyTable)
colSums(MyTable)

outer(c(1,2,3),c(4,8,16,32))
expected = outer(rowSums(MyTable), colSums(MyTable))/sum(MyTable)

########  R Diary of 10/26 #######

# Load the "mice" data from the USB drive.The  data are available in blackboard.
load("E:/ANLY511/Oct26/mice.rdata")

head(mice)
pop time
1 control   30
2 control   19
3 control   21
4 control   40
5 control   32
6 control   31

summary(mice)
pop          time      
control  :10   Min.   :15.00  
treatment:14   1st Qu.:18.75  
Median :21.50  
Mean   :23.29  
3rd Qu.:27.25  
Max.   :40.00  

# This is a data frame with two columns, 
# the first one is the the "control" or "treatment", 
# and the second one is a recorded time.

# Make side-by-side box plots:

boxplot(time ~ pop, data = mice)

# Compute the means of the two groups (control and treatment) in three different ways:

mean(mice$time[mice$pop == "control"])
[1] 28
> mean(mice$time[mice$pop == "treatment"])
[1] 19.92857

# With "aggregate". The result is a data frame.

aggregate(time ~ pop, data = mice, FUN = mean)
pop     time
1   control 28.00000
2 treatment 19.92857

# With tapply. The result is a named vector.

tapply(mice$time, mice$pop, FUN = mean)

control treatment 
28.00000  19.92857 

# tapply is substantially faster: 

system.time(for (j in 1:1000){tapply(mice$time, mice$pop, FUN = mean) -> dummy})
user  system elapsed 
0.14    0.00    0.14 

system.time(for (j in 1:1000){aggregate(time ~ pop, data = mice, FUN = mean) -> dummy})
user  system elapsed 
0.95    0.00    0.97 

# Compute the value of the test statistic (difference of means) 
# and store it in the variable t0. Drop the name.

x0 <- tapply(mice$time, mice$pop, FUN = mean)
x0[1] - x0[2] -> t0
> t0
control 
8.071429 
names(t0) <- NULL

# Now make a random permutation of the numbers 1, ..., 24..

perm <- sample(24,24, replace = F)
perm
[1] 24 12 20 14  5 17  7  8 10 23 21 19 16 13 22 11
[17]  3  1 15  6 18  4  9  2

# Make a data frame that has the same first column as the original one, 
# but the second column (times) is now permuted
# If the null hypothesis is true (no effect of the treatment), 
# then this is essentially how the control and treatment labels 
# were assigned to the different mice.

miceperm <- data.frame(pop = mice$pop, time = mice$time[perm])

# The box plots now show that after the permutation, 
# the two groups have comparable time distributions.

boxplot(time ~ pop, data = miceperm)

# We can also compute the two group means after the permutation. 
# They're very similar.

tapply(miceperm$time, miceperm$pop, mean)
control treatment 
23.7      23.0 

# Make a function that implements a permutation, 
# computes the group means, and then 
# returns the difference of the two group means.
# The function does not take any arguments.

micepermfun = function(){
  + perm <- sample(24,24, replace = F)
  + miceperm <- data.frame(pop = mice$pop, time = mice$time[perm])
  + x <- tapply(miceperm$time, miceperm$pop, mean)
  + y <- x[1] - x[2]
  + names(y) <- NULL
  + return(y)
  + }
  
# Try it out, it works.
  
> micepermfun()
[1] 1.042857

# Now do this 1000 times, and make a histogram of the simulated times.
# Draw a line at the observed test statistic.\

z <- replicate(10000,micepermfun())
> hist(z)
> lines(c(t0,t0), c(0,2000), col = 2)

# Very few simulated results are larger than the observed test statistic:

> sum(z > t0)
[1] 2

# We can compute the fraction of these cases:

> mean(z > t0)
[1] 2e-04

# This is a simulation result of the P value.
# It is the simulated probability that under the null hypothesis 
# values as large as the observed statistic or larger would occur.

# Make a larger simulation to get more accuracy

z <- replicate(50000,micepermfun())
hist(z)
lines(c(t0,t0), c(0,2000), col = 2)
sum(z > t0)
[1] 15

# A more accurate way of estimating the P value consists in 
# including the observed test statistic in the set of simulations. 
# This adds one in the numerator and denominator:

(sum(z > t0) + 1)/(length(z) + 1)
[1] 0.0003199936

# The P value is very small. 
# It is highly unlikely that a value such as the observed test statistic 
# would occur under the null hypothesis. 
# This is the very strong evidence that the null hypothesis is not 
# correct and the alternative is true.
# We therefore reject the null hypothesis in favor of the alternative.

# Read the Verizon repair data (also available in Blackboard).

Verizon <- read.csv("E:/ANLY511/Data/Verizon.csv")

# There are 23 repair times in the CLEC group 
# (customers of competing landline carriers) and 1664 repair times 
# in the ILEC group (customers of Verizon).

summary(Verizon)
Time          Group     
Min.   :  0.000   CLEC:  23  
1st Qu.:  0.750   ILEC:1664  
Median :  3.630              
Mean   :  8.522              
3rd Qu.:  7.350              
Max.   :191.600              

# Make side-by-side box plots. The two distributions are heavily skewed, 
# and in addition the CLEC group is much smaller than the ILEC group.
 
boxplot(Time ~ Group, data = Verizon)

# Mean and median repair times are both higher for the CLEC group.

tapply(Verizon$Time, Verizon$Group, mean)
CLEC      ILEC 
16.509130  8.411611 

tapply(Verizon$Time, Verizon$Group, median)
CLEC  ILEC 
14.33  3.59 

# The null hypothesis is: mean repair times are the same for the two types of customers.
# We can again examine this with a permutation test.
# As before, make a function that performs the permutation.

verizonpermfun = function(){
  + perm <- sample(1687,1687, replace = F)
  + verizonperm <- data.frame(Time = Verizon$Time[perm], Group = Verizon$Group)
  + x <- tapply(verizonperm$Time, verizonperm$Group, mean)
  + y <- x[1] - x[2]
  + names(y) <- NULL
  + return(y)
  + }
  
# Here's an alternative that will run faster. This was not done in class.

altverizonpermfun = function(){
  + index <- sample(23,1687, replace = F)
  + y <- mean(Verizon$Time[index]) - mean(Verzon$Time[-index])
  + return(y)
  + }
  
# Try it out, it works. It should give a different result each time.

verizonpermfun()
[1] 1.837389
> verizonpermfun()
[1] 4.011817

# Record the original difference of group means in the observed test statistic t0, as before.

tapply(Verizon$Time, Verizon$Group, mean) -> x1
t0 <- x1[1] - x1[2]
t0
CLEC 
8.09752 
names(t0) <-NULL

# Now simulate 10,000 permutations and compute the test statistic in each case.
# This gives us a simulation of the null distribution.

z <- replicate(10000, verizonpermfun())

# Make it histogram and find the proportion of simulated values 
# which are larger than the observed statistics.

hist(z)
(1+sum(z > t0))/(10001)
[1] 0.02089791

# This is quite small ("statistically significant"), 
# so there is fairly strong evidence that the mean repair time 
# for CLEC customers is larger than the mean repair time for ILEC customers.

# Redo this  for medians.
# First record the observed difference of medians:

x2 <- tapply(Verizon$Time, Verizon$Group, median)
t2 <- x2[1] - x2[2]
names(t2) <- NULL

# Define a permutation function. This is the same function as before, 
# except that we are now using median instead of mean.

verizonpermfun1 = function(){
  + perm <- sample(1687,1687, replace = F)
  + verizonperm <- data.frame(Time = Verizon$Time[perm], Group = Verizon$Group)
  + x <- tapply(verizonperm$Time, verizonperm$Group, median)
  + y <- x[1] - x[2]
  + names(y) <- NULL
  + return(y)
  + }

# Simulate 10,000 permutations to get the null distribution 
# of the test statistic (difference of medians).  
  
z1 <- replicate(10000, verizonpermfun1())
hist(z1)
(1+sum(z1 > t2))/(10001)
[1] 0.00109989

# This P value is much smaller. If the median is used (which might be appropriate, 
# since the repair times in the samples are heavily skewed), 
# the evidence that CLEC customers have longer repair times on average than 
# ILEC customers becomes much stronger.

# Redo this to get another estimate.
# Clearly, there is variability in the estimated P value.

z1 <- replicate(10000, verizonpermfun1())
hist(z1)
(1+sum(z1 > t2))/(10001)
[1] 0.00129987

# Examine birth months of World Cup soccer players

birth <- c(150,138,140,100)

# Make a vector of expected counts, assuming there is a uniform distribution.

expected <- rep(mean(birth),4)

expected
[1] 132 132 132 132

# Compute the test statistic (Pearson' s Chi-squared)

X2 = sum((birth - expected)^2/expected)
X2
[1] 10.9697

# Define a function that computes the test statistic, and try it out

mysoccerX2 = function(x){return(sum((x - expected)^2/expected))}
mysoccerX2(birth)
[1] 10.9697

# Now simulate 10000 birth month vectors, 
# assuming birth months are distributed uniformly. 
# This is done by simulating a random sample from a multinomial distribution.

sim <- replicate(10000, mysoccerX2(rmultinom(1,528,rep(1/4,4))))

# This is very fast. Make it histogram

hist(sim)
abline(v = X2, , col = 2, lwd = 2)

# The observed value of the test statistic appears far to the right in the tail.

# Compute the fraction of simulated test statistics which are larger than the observed value:

mean(sim > X2) # Well that's not quite correct, but it's convincing anyway.
[1] 0.0114

# This is therefore an estimate for the P value. It is very small, 
# therefore there is strong evidence that the null hypothesis is not correct. 
# The distribution of birth months across the four quarters is not uniform.

# Repeat, obtain a slightly different value.

sim <- replicate(10000, mysoccerX2(rmultinom(1,528,rep(1/4,4))))
mean(sim > X2)
[1] 0.0123

# The null distribution should have a chi-square distribution with 3 degrees  of freedom.
# Check this by making a probability histogram and drawing 
# the probability density function of this chi-square distribution in the same plot.

hist(sim, prob = T, breaks = 30)
xx <- seq(0,20,by=.1)
lines(xx,dchisq(xx,3), col = 2, lwd = 2)

# This is indeed very close. 
# We can therefore compute the P value also approximately from the chi-square Ccdf:

pchisq(X2,3, lower.tail=F)
[1] 0.01189087

# Work with the General Social Survey of 2006 data.

GSS2006 <- read.csv("E:/ANLY511/Data/GSS2006.csv")

# We are interested in the gender and happiness data in the table.

table(GSS2006$Gender)

Female   Male 
2507   2003 

# No missing data.

table(GSS2006$Happy)

Not too happy  Pretty happy    Very happy 
390          1676           920 

# Many missing data, the number of table entries does not add up to 4510.

# Now make a two way table ("cross tabulate")

table(GSS2006$Gender, GSS2006$Happy)

Not too happy Pretty happy Very happy
Female           227          939        527
Male             163          737        393

# We are interested in the question whether the different happiness values 
# are distributed in the same way for males and females.

table(GSS2006$Gender, GSS2006$Happy) -> MyTable

# Compute row sums and column sums:

rowSums(MyTable)
Female   Male 
1693   1293 

colSums(MyTable)
Not too happy  Pretty happy    Very happy 
390          1676           920 

# To make a table of expected counts, we must multiply each row sum entry 
# with each column sum entry and divide the result by the total count in the table.

# This can be done very simply with the "outer()" function.
# Illustration:

> outer(c(1,2,3),c(4,8,16,32))
[,1] [,2] [,3] [,4]
[1,]    4    8   16   32
[2,]    8   16   32   64
[3,]   12   24   48   96

# Here is the table of expected cell counts, assuming that happiness values 
# are distributed the same for both genders (the two populations are homogeneous). 

expected = outer(rowSums(MyTable), colSums(MyTable))/sum(MyTable)
expected

Not too happy Pretty happy Very happy
Female      221.1219     950.2572   521.6209
Male        168.8781     725.7428   398.3791

# This is very close to the observed. Here is the table of differences:

expected - MyTable

Not too happy Pretty happy Very happy
Female     -5.878098    11.257200  -5.379102
Male        5.878098   -11.257200   5.379102

# See the slides for the conclusion. 
# A permutation test shows that there is no evidence to reject the null hypothesis 
# that the two populations have the same distribution.

##############  End of R Diary for 10/26  ############