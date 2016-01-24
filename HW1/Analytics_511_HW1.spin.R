####Problem 1
sinResult = sin(1.23)
a = as.numeric(tail(unlist(strsplit(as.character(floor(sinResult*10^10)), "")), n = 1))
a

b = sqrt(a^2+a^3)
b 
#[1] 28.4605
c = nchar(floor(exp((log(b))^3)))
c
#[1] 17
d = sum((1:c)^3)
#[1] 23409

####Problem 2
mytoss = function(p){
  u = runif(1)
  x = as.numeric(u<p)
  return(x)
}

myattempts = function(p){
  counter <- 1
  while (mytoss(p) == 0){
    counter <- counter + 1
  }
  return(counter)
}
pValues = c(0.01, 0.25,0.5,0.75, 0.99)
problem2 = matrix(nrow = 2, ncol = length(pValues))
for(i in 1:length(pValues)){
  avgMyattemptsTimeTimes = c()
  avgRgeomTimes = c()
  myattemptsTime = mean(replicate(10000, system.time(myattempts(pValues[i]))[3]))  
  rgeomTimes = mean(replicate(10000, system.time(1 + rgeom(1,pValues[i]))[3]))
  
  problem2[1,i] = myattemptsTime
  problem2[2,i] = rgeomTimes
}
pValues
#   [1] "0.01" "0.25" "0.5"  "0.75" "0.99"
problem2          
#		[,1]      [,2]     [,3]     [,4]     [,5] 
#[1,] 0.0011956 0.0001309 9.32e-05 8.44e-05 7.12e-05 
#[2,] 0.0000642 #0.0000666 5.55e-05 5.91e-05 5.80e-05

####Problem 3
pValue = c(0.01, 0.2,0.4,0.6,0.8, 0.99)
estimatedpValue = 1:length(pValue)
for(i in estimatedpValue){
  estimatedpValue[i] <- sd(replicate(10000, 1+rgeom(1,pValue[i])))
}
actual_sd_value = sqrt(1-pValue)/pValue
####Problem 4
meanRexp = function(n){ 
  mean(rexp(n))
}
sapply ( rep (20 , times = 10) , meanRexp)
####Problem 6
1000/(100*10^6)
#[1] 1e-05

((100*10^6-1000)/(100*10^6))^2000 
#[1] 0.8187226

notInSample = function(n){
  ((100*10^6-1000)/(100*10^6))^n
}
n = 1
InSample = notInSample(1)

while(InSample>.50){
  n = n+1
  InSample = notInSample(n)
}
n
####Problem 7
problem7 = 1:100000
for(i in 1:100000){
aa = rgeom(4,.3)
problem7[i] = mean(aa^2)
}
phat = (sqrt(1+8*mean(problem7))-1)/(2*mean(problem7))
##Formula
phat - .3
#[1] 0.05359303

####Problem 8
startrunifs = mean(replicate(100000,runif(1, max = 1)))
print(startrunifs)
#[1] 0.7391124
EstimatesOfX = c(startrunifs)
for(i in 1:5){
  startrunifs = mean(replicate(100000, runif(1, max = startrunifs)))
  EstimatesOfX = c(EstimatesOfX,startrunifs)
}
tail(EstimatesOfX, n = 1)
#[1] 0.03814304