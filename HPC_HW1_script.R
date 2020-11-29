library(dplyr)
library(parallel)

#Pi.estimator is the estimation function
pi.estimator<- function (n){
  
  #Function of unit circle
  my.pi.func<- function(x) sqrt(1-x^2)
  
  # Generate n random numbers between 0 and 1 4 times, for ability to parallelize
  my.pi.unif.1<- runif(n/4,0,1)
  my.pi.unif.2<- runif(n/4,0,1)
  my.pi.unif.3<- runif(n/4,0,1)
  my.pi.unif.4<- runif(n/4,0,1)
  
  #apply function to random uniform distributions
  quarter.pi.vector.1<- sapply(my.pi.unif.1, FUN=my.pi.func)
  quarter.pi.vector.2<- sapply(my.pi.unif.2, FUN=my.pi.func)
  quarter.pi.vector.3<- sapply(my.pi.unif.3, FUN=my.pi.func)
  quarter.pi.vector.4<- sapply(my.pi.unif.4, FUN=my.pi.func)
  
  #Sum each vector, take mean over n
  quarter.pi<- sum(c(quarter.pi.vector.1,
                     quarter.pi.vector.2,
                     quarter.pi.vector.3,
                     quarter.pi.vector.4))/n
  
  #Multiple by 4 to get estimate of pi
  pi_est<- quarter.pi*4
  pi_est
}


#Create vector of n
n.min<- 100
n.max<- 1e6  ####<- set this value #####
n.index<- seq(n.min, n.max, by=100)

#Set seed
seed<- 123456
set.seed(seed)

##Alternative Approach using for loop
# #Create blank data frame
# estimate.table<- data.frame()
# 
# #Run pi.estimator against each value in n.index and bind to estimate table
# for (i in n.index){
#   estimate.table<- estimate.table%>%
#     bind_rows(c("n" = i,
#                 "estimate"=pi.estimator(n=i)))
# }


###Make cluster
cl <- parallel::makeCluster(100, setup_timeout = 0.5)

#Export pi.estimator to cluster
clusterExport(cl, "pi.estimator")

#Return estimate table of n as input into pi.estimator
estimate.table<- data.frame("n"=n.index,
                            "estimate"= parSapply(cl, n.index, 
                                                  FUN=function(x) pi.estimator(x)))



################Pick sufficient n above#####################
#Set n to minimum n above where pi is correct to digs digits
digs<- 5
pi.estimate<- 3.14159
n<-median(estimate.table$n[round(estimate.table$estimate,digs)==pi.estimate])

#Replicate pi.estimator 100 times
replication.table<- replicate(100, pi.estimator(n))

#Compare to pi.estimate and calculate accuracy 
accuracy<- mean(round(replication.table,digs)==pi.estimate)

#Output
output1<- tibble("Sufficient n" = n,
                 "Reliability @ n" = accuracy)

#RDS
saveRDS(output1, "Output Problem 1.RDS")

#Stop Cluster
stopCluster(cl)
