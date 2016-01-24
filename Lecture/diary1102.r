############   R Diary for 11/02/15   ###########

# We worked with the GSS2006 data.
# Load the dataset and make tables of Happy and Gender entries.
 
> GSS2006 <- read.csv("E:/ANLY511/Data/GSS2006.csv")
> table(GSS2006$Gender)

Female   Male 
2507   2003 

> table(GSS2006$Happy)

Not too happy  Pretty happy    Very happy 
390          1676           920 

> mytable <- table(GSS2006$Gender, GSS2006$Happy)
> mytable

Not too happy Pretty happy Very happy
Female           227          939        527
Male             163          737        393

# Compute expected counts. This is still review from last week.

> expected = outer(rowSums(mytable), colSums(mytable))/sum(mytable)
> expected
Not too happy Pretty happy Very happy
Female      221.1219     950.2572   521.6209
Male        168.8781     725.7428   398.3791

# Define a function that computes the chi-square statistic
# and evaluate it for the given table.

> myX2 <- function(table){sum((table - expected)^2/expected)}
> X2 <- myX2(mytable)
> X2
[1] 0.7969291

# If this value is large, it indicates relatively large discrepancies 
# between expected and observed values. This would be evidence that happiness levels 
# are not distributed the same way between the two populations. 
# To determine what is "large", we use permutations 
# to simulate the null distribution of the chi-square statistic.

# First extract those rows from the original data set where all data are complete.
# If this is not done, the row totals (numbers of males and females) 
# may change due to random permutations.
# Then the expected cell counts would no longer be correct.

> SubGSS2006 <- GSS2006[!is.na(GSS2006$Happy),]

# Make a function that permutes the Happiness column entries randomly, 
# extracts a two-way table, and then computes the chi-square statistic.
The dummy argument is not needed if the simulation is done with "replicate", 
# but may be necessary if the simulation is done with "sapply".

> X2permuted = function(dummy){
  + n <- length(SubGSS2006[,1])
  + permtable <- table(SubGSS2006$Gender,
                       + SubGSS2006$Happy[sample(n,n,replace = F)])
  + return(myX2(permtable))
  + }

# Check if it works. It produces values similar to the observed chi-square value. 
 
 > X2permuted(1)
[1] 2.39676
> X2permuted(1)
[1] 1.879501
> X2permuted(1)
[1] 0.7800361

# Now make 10,000 random permutations and compute the chi-squared statistic in each case.

> z <- replicate(10000,X2permuted(0))

# The histogram shows that the observed value of .7969 is not particularly large.

> hist(z)
> abline(v = X2, , col = 2, lwd = 2)

# Computes the fraction of z's that are larger than the observed value. This is essentially the P value.

> mean(z > X2)
[1] 0.6735

# This is NOT small, therefore we cannot reject the null hypothesis.
# There is no evidence in the data that the distribution of happiness values is depending on gender.
# Common beginner's mistake: "Reject the null hypothesis because the P value is large".

# The null distribution is a chi-square distribution with 2 degrees of freedom, 
# which is the same as an exponential distribution with lambda = 1/2. 
# For example, the mean is approximately two:

> mean(z)
[1] 2.022583

# We can therefore compute the P value approximately 
# from the cdf of a chi-square distribution, with 2 degrees of freedom.

> pchisq(X2,2,lower.tail = F)
[1] 0.6713501

# Note that this differs from the simulated value. 
# This does not affect the decision (the P value is too large 
# to reject the null hypothesis).

# Both computations (permutation simulation and chi-square computation) 
# are approximations, one from a random simulation, the other from an 
# analytic approximation formula. This does not affect the decision "Cannot reject" at all.

# R Has a built in function for chi-square tests for a given table.
# The output is a list.

> chisq.test(mytable) -> results

# Explore the list. Note that R does not automatically generate output 
# with all the results, in contrast to SAS which tends to generate a lot of output.

> names(results)
[1] "statistic" "parameter" "p.value"   "method"    "data.name"
[6] "observed"  "expected"  "residuals" "stdres"   

# Here is the chi-square statistic X2 for the original table:

> results$statistic
X-squared 
0.7969291 

# Degrees of freedom for the chi-square distribution, (r-1)*(c-1)

> results$parameter
df 
2 
> results$p.value
[1] 0.6713501
> results$method
[1] "Pearson's Chi-squared test"
> results$observed

Not too happy Pretty happy Very happy
Female           227          939        527
Male             163          737        393
> 
  > results$expected

Not too happy Pretty happy Very happy
Female      221.1219     950.2572   521.6209
Male        168.8781     725.7428   398.3791
> 

# The residuals are the terms (observed - expected)/sqrt(expected) for each cell. 
# The sum of the squares of these terms, equals the chi-square statistic X2.
# These are sometimes called Pearson residuals.

# Any entries which are particularly large make the X2 statistic large, 
# and therefore lower the P value. So large Pearson residuals indicate 
# where the homogeneity assumption for the distributions is violated.

  > results$residuals

Not too happy Pretty happy Very happy
Female     0.3952946   -0.3651824  0.2355225
Male      -0.4523245    0.4178679 -0.2695017

# An alternative  way to look at discrepancies between observed and expected 
# cell counts is with adjusted standardized residuals. 
# R reports these values as a table called stdres. 
# These are the Pearson residuals, divided by estimated standard errors of the observed counts.
# In a table with two rows, this implies that the two elements in in any column 
# have the same magnitude and opposite signs.
# These values can be interpreted as if they came from observations 
# of a standard normal distribution. For example, values with magnitude > 2 
# are expected to occur with probability of about 5%, and 
# values of magnitude > 3 should be extremely rare (less than half a percent).

> results$stdres

Not too happy Pretty happy Very happy
Female     0.6442570   -0.8378470  0.4302865
Male      -0.6442570    0.8378470 -0.4302865

> sum(results$residuals^2)
[1] 0.7969291

# R will carry out a Pearson chi-square test even if the expected cell counts 
# are too small to justify this approximation. 
# In that case you get a warning message, such as in this case:

> chisq.test(matrix(c(1,2,3,4,5,4,3,4),ncol = 2)) -> results
Warning message:
  In chisq.test(matrix(c(1, 2, 3, 4, 5, 4, 3, 4), ncol = 2)) :
  Chi-squared approximation may be incorrect
> results

Pearson's Chi-squared test

data:  matrix(c(1, 2, 3, 4, 5, 4, 3, 4), ncol = 2)
X-squared = 2.0583, df = 3, p-value = 0.5604

> results$expected
[,1]     [,2]
[1,] 2.307692 3.692308
[2,] 2.307692 3.692308
[3,] 2.307692 3.692308
[4,] 3.076923 4.923077

############ The Bootstrap Idea ##########

# Make a sample of size 20 from the standard uniform distribution.
# These are the only data we will work with.
# No more data will be used.

> x <- rnorm(20)

# Take a look at the sample mean.

> mean(x)
[1] -0.02581952

# A bootstrap sample is a sample, drawn with replacement from the given sample, of the same size.
# If the size of the given sample  x  is  nx, then a bootstrap sample is made with the command 
# sample(x,nx,replace = T)

# Make 100,000 bootstrap samples, compute the mean of each bootstrap sample, and store the results
# The mean of these 100,000 bootstrap sample means is very close to the original sample mean. 

> z <- replicate(100000, mean(sample(x,20, replace = T)))
> mean(z)
[1] -0.02724287
> mean(x)
[1] -0.02581952

# The standard deviation of these 100,000 bootstrap sample means 
# is close to the standard deviation of the mean of a sample of size 20, 
# drawn from a standard normal distribution.
# We know that the sample mean of samples of size 20 
# from a standard normal distribution has a normal distribution 
# with mean 0 and standard deviation 1/sqrt(20).

> sd(z)
[1] 0.2448898
> 1/sqrt(20)
[1] 0.2236068

# Examine the distribution of the 100,000 bootstrap sample means further, 
# with histogram and ecdf:

> hist(z, breaks = 50)
> plot.ecdf(z)

# Plot the theoretical distribution of a sample mean in the same plot:

> ecdf.theory <- function(x) pnorm(x,0,1/sqrt(20))
> ecdf.x <- ecdf(x)
> xx <- seq(-1,1,by=.01)
> lines(xx,ecdf.theory(xx), col = 2, lwd = 2)

# We can see that the two distributions are really very close.
# That is, the bootstrap distribution of a statistic 
# (which comes from a single sample) is usually quite close 
# to the actual distribution of that statistic 
# (which comes theoretically from infinitely many samples).

# Uses of the bootstrap: estimating and reducing bias.
# Define the plug-in estimator for the variance of a population.
# It is known that this estimator is biased. 
# The sum of squares should be divided by n-1, not by n.
# Therefore, the plug-in estimator usually gives a value that's too small. 
# The bias is negative.

> myvar = function(y){ mean((y-mean(y))^2)}
> myvar(x)
[1] 1.202463
> var(x)
[1] 1.26575

# In this case, we know exactly what the bias correction should be. 
# We should add  var(x) - myvar(x)  to the value from the plug-in estimator.
# What if we don't know what to do? The bootstrap can give us an idea of the bias.

# Make a bootstrap distribution of the plug-in estimator for the sample.
# That is, make many bootstrap samples, compute the plug-in estimator 
# for each of them, and store the result.

> z <- replicate(10000, myvar(sample(x,20, replace = T)))

# Since we are treating the sample (replicated many times) 
# as if it were the original population, 
# we know exactly what the variance is. It is myvar(x). 
# What is the average value of the plug-in estimator in the situation? 

> mean(z)
[1] 1.142624
> myvar(x)
[1] 1.202463

# This says that the plug-in estimator on average gives us a number that is too small. 
# So it correctly detects the fact that the plug-in estimator has a negative bias.

# The difference of these two quantities is the estimated bias:

> estimatedbias =  mean(z) - myvar(x) 
> estimatedbias
[1] -0.05983898

# Compare this to the true bias:

> truebias = myvar(x) - var(x)
> truebias
[1] -0.06328751

# We can therefore correct the value that is obtained by the plug-in estimator:

> correctedvar = myvar(x) - estimatedbias
> correctedvar
[1] 1.262302

# This is very close to the value that will be obtained by using the theoretically correct formula.

> var(x)
[1] 1.26575

# Bootstrap Confidence Intervals
# We want to give a confidence interval for the mean of a population.
# This is an interval where we can say 
# "if we use this particular method, it will give us an interval 
# that contains the true population mean in X% of all cases".
# It does NOT mean that the true population mean is contained in this interval with X% probability.
# The true population mean either is contained in the confidence interval, 
# or it is not contained in it. That's not a random event!

# Load the Verizon customer service data.
# Extract the service times for ILEC customers to a separate vector.
> Verizon <- read.csv("E:/ANLY511/Data/Verizon.csv")
> ilec <- Verizon$Time[Verizon$Group=="ILEC"]

# The data are badly skewed. The mean is about 8.41 hours.

> hist(ilec)
> mean(ilec)
[1] 8.411611

# We would like to make a confidence interval for the main service time.
# Since we don't know anything about the distribution of the original data 
# and cannot obtain more samples, 
# we make a bootstrap simulation of the distribution of the sample mean.

> z <- replicate(10000, mean(sample(ilec, 1664, replace = T)))
> hist(z,breaks =30)
> qqnorm(z)

# The histogram shows a bell-shaped distribution.
# The quantile quantize plot confirms that this is approximately normal.

# We would like to find an interval that contains 95% of the simulated sample means.
# Simulated distribution and find out where bottom 2.5% and the top 2.5% are cut off.

> z <- sort(z)
> z[250]
[1] 7.716256
> z[9750]
[1] 9.136809

# Therefore, a 95% confidence interval for the mean customer service time is [7.71, 9.13]. 
# The length of this interval is approximately 1.4.

# Repeat this with the CLEC data (customers from competing carriers).
# Note that the sample is much smaller (only 23 entries).

> clec <- Verizon$Time[Verizon$Group=="CLEC"]
> mean(clec)
[1] 16.50913
> hist(clec)
> z1 <- replicate(10000, mean(sample(clec, 23, replace = T)))
> hist(z1, breaks = 30)
> qqnorm(z1)

# Definitely not normally distributed.

# Make a confidence interval as before:

> z1 <- sort(z1)
> z1[250]
[1] 10.07
> z1[9750]
[1] 25.58304

# The 95% confidence interval for the mean customer service time
# for CLEC customers is approximately [10, 25]. It contains the mean 16.5, but it is not symmetric about it.
# That is because the distribution of bootstrap sample means is not symmetric. 
# It is approximately 10 times as long as the confidence interval for the ILEC customers, 
# which is consistent with the fact that the sample size is about 1/100  
# of the sample size for the ILEC customers. 

##################  End of R Diary for 11/02 ###############