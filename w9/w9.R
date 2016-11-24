# Load the NHPoisson package
install.packages('NHPoisson')
library(NHPoisson)
# Read in the scoring time data, rugby times (0-80 minutes)
x <- c(3,4,10,17,21,23,34,49,52,57,59,64,76)
# Fit a homogeneous Poisson process
# b0 = log of intenity lambda
fit0 <- fitPP.fun(n=80,posE=x,start=list(b0=0))
summary(fit0)


# Allow for a linear intensity function
# Set up the time covariate (I have made this 1 to 80 in 1 minute intervals)
timerange<-seq(1,80,by=1)
covariates<-timerange
#Fit the inhomogeneous Poisson process
fit1 <- fitPP.fun(covariates=covariates,posE=x,start=list(b0=0,b1=0))
summary(fit1)

# Allow for a quadratic intensity function
timerange<-seq(1,80,by=1)
covariates<-cbind(timerange,timerange^2)
#Fit the inhomogeneous Poisson process
fit2 <- fitPP.fun(covariates=covariates,posE=x,start=list(b0=0,b1=0,b2=0))
summary(fit2)


#Load the lpint package
install.packages('lpint')
library(lpint)
# Read in the scoring time data
x <- c(3,4,10,17,21,23,34,49,52,57,59,64,76)
# Set up the number of scores at each time point
#Set to be 1 in this case
y<-rep(1,length(x))
#Fit the inhomogeneous Poisson process
fit <- lpint(jmptimes=x,jmpsizes=y,Tau=80)
plot(fit,type="l",xlab="Time",ylab="Intensity")
summary(fit)

# Read in data (file downloaded from the Capital bikeshare scheme website)
dat0 <- read.csv("~/Downloads/2016-Q3-cabi-trips-history-data/2016-Q3-Trips-History-Data-1.csv")
# Extract data on collections from station 31200
dat <- dat0[dat0$Start.station.number==31200,]
# Extract the relevant events (found manually)
dat<-dat[10510:9345,]
# Extract the event times
x<- as.character(dat$Start.date)
# Put into numeric format (seconds from start at midnight 7/2/2016)
library(lubridate)
x <- mdy_hm(x)
x <- as.numeric(x)
x <- x-as.numeric(mdy_hm("7/2/2016 0:00"))
# Set up the number of scores at each time point
# Set to be 1 in this case
y<-rep(1,length(x))
#Fit the inhomogeneous Poisson process
fit <- lpint(jmptimes=x,jmpsizes=y,Tau=7*24*60*60)
# Plot the fit
plot(fit,type="l",xlab="Date",ylab="Intensity",axes=FALSE)
abline(v=(0:7)*(24*60*60),col="darkgray",lty=3)
axis(1,at=((0:6)+0.5)*(24*60*60),labels=c(2:8))
axis(2)