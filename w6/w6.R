# Set up a grid of time values
tvec <- seq(0,15,length=101)
# Set the parameters of the distribution
alpha <- 2
beta <- 2
# Plot the density, CDF, Survival and hazard
par(mfrow=c(2,2))
# Density
plot(tvec,dgamma(tvec,shape=alpha,scale=beta),type="l",ylab="Density")
# CDF
plot(tvec,pgamma(tvec,shape=alpha,scale=beta),type="l",ylab="CDF")
# Survival
plot(tvec,1-pgamma(tvec,shape=alpha,scale=beta),type="l",ylab="Survival")
# Hazard
plot(tvec,dgamma(tvec,shape=alpha,scale=beta)/(1-pgamma(tvec,shape=alpha,scale=beta)),type="l"
     ,ylab="Hazard")
par(mfrow=c(1,1))



####Weibull Dist
# Set up a grid of time values
tvec <- seq(0,10,length=101)
# Set the parameters of the distribution
alpha <- 3
beta <- 4
# Plot the density, CDF, Survival and hazard
par(mfrow=c(2,2))
# Density
plot(tvec,dweibull(tvec,shape=alpha,scale=beta),type="l",ylab="Density")
# CDF
plot(tvec,pweibull(tvec,shape=alpha,scale=beta),type="l",ylab="CDF")
# Survival
plot(tvec,1-pweibull(tvec,shape=alpha,scale=beta),type="l",ylab="Survival")
# Hazard
plot(tvec,dweibull(tvec,shape=alpha,scale=beta)/(1-pweibull(tvec,shape=alpha,scale=beta)),type="l",ylab="Hazard")
par(mfrow=c(1,1))


### Read in Failure Times
x <- c(79, 105, 14, 153, 67, 25, 39, 9, 55, 132, 48, 570)
# Form log-likelihood
logl <- function(lambda){
  sum(dexp(x,lambda,log=TRUE))
}
# Initial parameter
lambda0 <- 1/mean(x)
#Optimization
fit <- optim(lambda0,logl,control=list(fnscale=-1),method="BFGS",hessian=TRUE)
#Parameters
lambdahat <- fit$par
SE <- sqrt(-1/fit$hess[1,1])
#95% CI
print(c(lambdahat-2*SE,lambdahat+2*SE))

y <- sapply(x,function(i){
  return(ifelse(i>50,1,0))
})

y.mean <- mean(y)

lambda.hat <- log(y.mean)/-50

##############################################################################
##############################################################################
##############################################################################

#Load survival package
library(survival)
#Load leukemia data
data(leukemia)
#Extract data for Maintained group
dat<-leukemia[leukemia$x=="Maintained",1:2]
dat
#Set up Surv() object
survdat <- Surv(time=dat[,1],event=dat[,2])
survdat
#Fit the model to the data
fit1 <- survreg(survdat~1,dist="exponential")
lambda <- 1/exp(fit1$coef)
#Study fit
summary(fit1)
#Plot the fit
tvec<-seq(0,200,length=201)
plot(tvec,dexp(tvec,lambda),type="l")

#Fit the model to the data
fit2 <- survreg(survdat~1,dist="weibull")
#Study fit
summary(fit2)
#Write parameters in original form
alpha <- 1/fit2$scale
beta <- exp(fit2$coef)
#Plot the fitted curves
tvec<-seq(0,200,length=201)
plot(tvec,dexp(tvec,lambda),type="l")
points(tvec,dweibull(tvec,alpha,beta),type="l",col="blue",lty=3)





#######################################################################
# Read in Failure Times
x <- c(79, 105, 14, 153, 67, 25, 39,9, 55, 132, 48, 570)
#Fit Kaplan-Meier Curve
library(survival)
fit <- survfit(Surv(x)~1,se=FALSE)
# Plot the fit
plot(fit)
#Add the exponential model fit
lambda<-1/mean(x)
tvec<-seq(0,1000,length=201)
points(tvec,1-pexp(tvec,lambda),type="l",col="blue",lty=3)


#######################################################################
#######################################################################

#Load survival package
library(survival)
#Load leukemia data
data(leukemia)
#Extract data for Maintained group
dat<-leukemia[leukemia$x=="Maintained",1:2]
dat
#Set up Surv() object
survdat <- Surv(time=dat[,1],event=dat[,2])
survdat
#Fit the KM to the data
fit <- survfit(survdat~1,se=FALSE)
summary(fit)
plot(fit)