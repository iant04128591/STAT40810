# Saxony data
n <- 0:8
f <- c(215,1485,5331,10649,14959,11929,6678,2092,342)
x <- rep(n,f)
y<-8-x
dat<-cbind(x,y)
colnames(dat)<-c("M","F")
# Load relevant package (flexmix)
install.packages('mixtools')
library(mixtools)
# Fit a G component mixture model
G<-1
fit<-multmixEM(dat,k=G)
# Examine the fit
summary(fit)

# Saxony data
n <- 0:8
f <- c(215,1485,5331,10649,14959,11929,6678,2092,342)
x <- rep(n,f)
y<-8-x
dat<-cbind(x,y)
colnames(dat)<-c("M","F")
# Load relevant package (mixtools)
library(mixtools)
# Fit a G component mixture model
G<-1
fit<-multmixEM(dat,k=G)
# Examine the fit
summary(fit)


#Load the BayesLCA package
install.packages('BayesLCA')
library(BayesLCA)
# Load the data
data(Alzheimer)
#Fit the G=2 model
fit2 <- blca.em(Alzheimer, 2)
fit2
#Fit the G=3 model
fit3<- blca.em(Alzheimer, 3, restarts=25)
fit3

# Load the flexmix package
install.packages('flexmix')
library(flexmix)
# Load the CO_2 data
data(CO2data)
# Fit a mixture of experts model with 50 random starting values for the EM algorithm.
# The highest BIC value is stored as bicval and the best fitting model as bestfit
bicval <- Inf
itermax <- 50
for (iter in 1:itermax)
{
  fit<-flexmix(CO2~GNP,data=CO2data,k=2)
  if (bicval>BIC(fit))
  {
    bicval<-BIC(fit)
    bestfit<-fit
    print(c(iter,bicval))
  }
}
# Explore the fitted model
summary(bestfit)
parameters(bestfit)


set.seed(1)
# Load the CO_2 data
library(flexmix)
data(CO2data)
# Load the mixtools package
library(mixtools)
# Fit a mixture of experts model with 50 random starting values for the EM algorithm.
# The highest BIC value is stored as bicval and the best fitting model as bestfit
bicval <- -Inf
itermax <- 50
for (iter in 1:itermax)
{
  G<-2
  fit<-regmixEM(CO2data$CO2,CO2data$GNP,k=G)
  n<-nrow(CO2data)
  p<-nrow(fit$beta)*G+G+(G-1)
  fitbic <- 2*fit$loglik - log(n)*p
  if (bicval<fitbic)
  {
    bicval<-fitbic
    bestfit<-fit
    print(c(iter,bicval))
  }
}
# Explore the fitted model
summary(bestfit)
plot(bestfit,which=2)