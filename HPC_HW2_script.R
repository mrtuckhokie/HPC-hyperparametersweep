library(dplyr)

#Pi Beer
####Pi Beer Approach
n <- as.numeric(n) #####<- set n from problem 1
set.seed(seed) ###set seed from problem 1


pi.beer<- function(){
  x.pos <- runif(n,0,10)
  y.pos <- runif(n,0,10)
  rotation <- runif(n,0,pi/2)
  # figure out the x,y coords of the match endpoints
  x.max <- x.pos + 0.5* cos(rotation)
  x.min <- x.pos - 0.5* cos(rotation)
  y.max <- y.pos + 0.5* sin(rotation)
  y.min <- y.pos - 0.5* sin(rotation)
  crosses <- ifelse(ceiling(x.min)==floor(x.max),1,0)
  # draw the board
  
  pi_est <- 2*n/sum(crosses)
  pi_est
}

replication.beer<- replicate(100, pi.beer())


#Pi Darts
####Pi Darts Approach
set.seed(seed) ##Set seed from problem 1
n <- as.numeric(n) #set n from problem 1

pi.darts<- function(){
  options(digits=15)
  point_container <- matrix(0,nrow=n, ncol=3)
  i <- 0
  while(i<n){
    i <- i+1
    current_point <- runif(n=2,min=0,max=1) #staying in first quad
    dist_from_origin <- sqrt(sum(current_point^2))
    point_container[i,] <- c(current_point,ifelse(dist_from_origin>1,0,1))
  }
  our_pi <- 4*sum(point_container[,3])/n
  our_pi
}

replication.darts<- replicate(100, pi.darts())


####Summary Table
#list of pi from 1 to 6 decimal points
pi.list<- c(3.1, 3.14, 3.141, 3.1415, 3.14159, 3.141592)

#Create accuracy table rounding rep table to y digits and comparing to yth digit of pi.list
accuracy.table.beer<- data.frame()
for (i in 1:length(pi.list)){
  accuracy.table.beer<- accuracy.table.beer%>%
    bind_rows(c("Pi Beer Accuracy"= mean(round(replication.beer,i)==pi.list[i])))
}

accuracy.table.darts<- data.frame()
for (i in 1:length(pi.list)){
  accuracy.table.darts<- accuracy.table.darts%>%
    bind_rows(c("Pi Darts Accuracy"= mean(round(replication.darts,i)==pi.list[i])))
}

#Bind all values
output2<- cbind("Pi values" = pi.list, accuracy.table.beer, accuracy.table.darts)

#RDS
saveRDS(output2, "Output Problem 2.RDS")

