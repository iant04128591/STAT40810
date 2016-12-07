# Load library for truncated normal
# install.packages('truncnorm')
library(truncnorm)

rm (list=ls())
#dev.list()
#dev.off()
#dev.on()

getwd()
list.files()
# Load the data
load("length.Rdata")

# Plot the data
hist(x,main="Histogram of length",prob=TRUE)

# Find likelihood
l <- function(param,a=30,b=60)
{
	mu <- param[1]
	sigma <- param[2]
	sum(log(dtruncnorm(x,a=a,b=b,mean=param[1],sd=param[2])))
}

# Compute MLE for normal model
n<-length(x)
mean0 <- mean(x)
sd0 <- sd(x)*sqrt((n-1)/n)
l0 <- sum(dnorm(x,mean0,sd0,log=TRUE))

# Compute MLE for truncated normal model
param0 <- c(mean0,sd0)
fit <- optim(param0,l,control=list(fnscale=-1),hessian=TRUE)
param <- fit$par
ObsInf <- - fit$hessian

# Plot data and fits
hist(x,main="Histogram of Length",prob=TRUE,xlim=c(30,65))
xgrid<-seq(30,70,length=1001)
points(xgrid,dnorm(xgrid,mean0,sd0),type="l",col="red")
points(xgrid,dtruncnorm(xgrid,a=30,b=60,mean=param[1],sd=param[2]),col="blue",lty=3,type="l")