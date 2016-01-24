# R Diary of 9/14/15

# Sample of size 6 from the set {1,2,...,49} (by default without replacement)

 sample(49,6)
[1] 37 30 19  4 16 45

# Sample with replacement. Repeated entries in the sample are now possible.

 sample(49,6, replace =T)
[1] 17 44 16 16  5 31

# Read in the baby names text file (available in Blackboard) and change the names. 
# Then take a look at the file.

 yob1990 <- read.csv("E:/ANLY511/sept14/babynames/yob1990.txt", header=FALSE, stringsAsFactors=FALSE)
 names(yob1990) <- c("name","gender","count")
 head(yob1990)
name gender count
1  Jessica      F 46466
2   Ashley      F 45549
3 Brittany      F 36535
4   Amanda      F 34406
5 Samantha      F 25864
6    Sarah      F 25808

# How many male names and how many female names?

 table(yob1990$gender)

F     M 
15231  9482 

# Make a sample of size 30 of names from babies born in 1990.

 elclass <- sample(yob1990$name, 30, prob = yob1990$count)
 elclass
[1] "Christian" "Ashley"    "Rachel"    "Laina"    
[5] "Steven"    "Alex"      "Olivia"    "Rebecca"  
[9] "Tera"      "Lauren"    "Carlos"    "Chanelle" 
[13] "Sarah"     "Casey"     "Allison"   "Michael"  
[17] "Dylan"     "Mikhail"   "Lachanda"  "Jared"    
[21] "Megan"     "Jordyn"    "Justin"    "Brittany" 
[25] "Katherine" "Mitchell"  "Cynthia"   "Jessica"  
[29] "Meghan"    "Johnny"   

# Repeat, with replacement, since there might be several children with the same first name in a class.

 elclass <- sample(yob1990$name, 30, prob = yob1990$count, replace = T)
 sort(elclass)
[1] "Alvin"       "Angela"      "Boyd"        "Caroline"   
[5] "Chaim"       "Christian"   "Christopher" "Connor"     
[9] "Courtney"    "Daniel"      "Erin"        "Geoffrey"   
[13] "Ian"         "Kala"        "Kevin"       "Kyle"       
[17] "Martez"      "Mattie"      "Michael"     "Natalie"    
[21] "Nathaniel"   "Randall"     "Rebecca"     "Ryan"       
[25] "Samuel"      "Sean"        "Shawn"       "Vashti"     
[29] "Victoria"    "Yolanda"    

# Compute the actual probability of each name from the count vector 
# and write it in a new column in the data frame.

 yob1990$prob <- yob1990$count/sum(yob1990$count)

# Note that some names are very rare. For example,
# There are only 25 children with first name "Kelcee" who were born in the US in that year.

 yob1990[yob1990$name == "Kelcee",]
name gender count        prob
4060 Kelcee      F    25 6.32871e-06

# Some names appear both as male and female first names.

 yob1990[yob1990$name == "Dana",]
name gender count         prob
114   Dana      F  2932 0.0007422311
15762 Dana      M   347 0.0000878425

# How common was your first name?

 yob1990[yob1990$name == "Arif",]
name gender count         prob
19021 Arif      M    15 3.797226e-06
 yob1990[yob1990$name == "Jordan",]
name gender count        prob
59    Jordan      F  5954 0.001507246
15260 Jordan      M 16128 0.004082778

# Work with binomial distribution
# 1 random number, n = 20, p = 2/7

 rbinom(1,20,2/7)
[1] 3
 rbinom(1,20,2/7)
[1] 8

# Make 10 random numbers

 rbinom(10,20,2/7)
[1] 8 3 6 7 2 7 5 6 8 3

# Probability mass function for x = 7

 dbinom(7,20,2/7)
[1] 0.1518004

# Cumulative distribution function for x = 7

 pbinom(7,20,2/7)
[1] 0.8138364

# Quantile for x = .2
# This means that P(X < 4 ) <=  .2  and P(X <= 4) = .2.

 qbinom(.2,20,2/7)
[1] 4
 pbinom(4,20,2/7)
[1] 0.2825349
 pbinom(3,20,2/7)
[1] 0.1342923

# Make a histogram of a large random sample.

 x <- rbinom(1000,20,2/7)
 hist(x)

# A better view: plot a table.

 table(x)
x
0   1   2   3   4   5   6   7   8   9  10  11  12  13 
1   9  32  83 162 172 192 174  94  51  19   8   2   1 
 plot(table(x))

# Now compute the probability mass function for all values from 0 to 20 
# and plot it in the same way.

 x1 <- 0:20
 plot(x1,dbinom(x1,20,2/7), type = 'h')

# The plots are similar, but of course not the same.

# Make a plot of the cumulative distribution function as a staircase.

 plot(x1,pbinom(x1,20,2/7), type = 's')

# We can also plot the "empirical cumulative distribution function"
# of the random sample  x  that was created earlier.

 plot.ecdf(x)

# By itself, the function ecdf() creates a function from a given sample 
# that can be used to compute the empirical cumulative distribution function 
# for any argument.

 z <- ecdf(x)

# 45.9% of the sample is <= 5.

 z(5)
[1] 0.459

# The theoretical value is 47.23%.

 pbinom(5,20,2/7)
[1] 0.4722854

# We can plot the empirical cumulative distribution function 
# against the cumulative distribution function. 
# The result is nearly a straight line.

 plot(z(x), pbinom(x,20,2/7))

# Compare a binomial distribution with large n and small p
# with a Poisson distribution with lambda = np
# using a simulation.

 xbinom <- rbinom(1000,100,.05) # Make a binomial random sample of size 1000
 xpois <- rpois(1000,5) # Make a possible random sample of the same sides

# Histograms of the two distributions. The distributions are very similar.

 plot(table(xbinom))
 plot(table(xpois))

# Now let's compare the theoretical probabilities.

 x <- 0:20 # These are the values at which distributions will be evaluated
 plot(x,ppois(x,5), type = 's') # Staircase plot of the cumulative distribution function of the Poisson distribution
 points(x,pbinom(x,100,.05), col = 2, lwd = 3) # Plot the values of the cdf of the binomial distribution in the same plot
 points(x,ppois(x,5), col = 4, lwd = 2) # Plot the values of the cdf of the Poisson distribution in the same plot again

# The plots show that the two distributions are very similar.

# Work with exponential distribution. Random sample of size 1000 and plot of the empirical cdf:

 x1 <- rexp(1000,1)
          plot.ecdf(x1)
		 
# Make two more samples from the same distribution		 

 x2 <- rexp(1000,1)
 x3 <- rexp(1000,1)

# We want to make a new random variable which consists of the minimum of x1, x2, x3.
# Here's how this can be done with a for loop:

 xmin <- rep(0,1000)
 for (j in 1:1000) xmin[j] <- min(c(x1[j], x2[j], x3[j]))

# Alternative, using matrix operations and sapply (not done in class)

A <- matrix(c(x1,x2,x3),ncol = 3)
mymin = function(j) min(A[j,])
xmin1 <- sapply(1:1000,mymin)

# The empirical cdf and the histogram suggest that this has also an exponential distribution.

 plot.ecdf(xmin)
 hist(xmin,breaks = 20)

# Do the same thing for maxima, first with a for loop.

 xmax <- rep(0,1000)
 for (j in 1:1000) xmax[j] <- max(c(x1[j], x2[j], x3[j]))

# Alternatively, using matrix operations and sapply

mymax = function(j) max(A[j,])
xmax1 <- sapply(1:1000,mymax)

# Plot the empirical cdf and make a histogram.
# This is not an exponentially distributed random variable.

 plot.ecdf(xmax)
 hist(xmax,breaks = 20)

# Finally, compute the sums of x1, x2, x3. First with a for loop:

 xsum <- rep(0,1000)
 for (j in 1:1000) xsum[j] <- sum(c(x1[j], x2[j], x3[j]))

# Or with a built-in function that computes row sums:

xsum1 <- rowSums(A)

# The plots show that this is again not an exponentially distributed random variable.

 hist(xsum,breaks = 20)
 plot.ecdf(xsum)

########### End of diary of 9/14/15