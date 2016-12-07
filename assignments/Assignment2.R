# Load the World Development Index package (WDI)
#install.packages('WDI')
library(WDI)
# Select the indices to be included in the study
# ind1 is the index for Health expenditure per capita
# ind2 is the index for GDP per capita
# ind3 is Life expectancy at birth
ind1 <- "SH.XPD.PCAP.PP.KD"
ind2 <- "NY.GDP.PCAP.KD"
ind3 <- "SP.DYN.LE00.IN"

# You can look for other indices using the WDIsearch() command.
WDIsearch(string="debt",field="name")
# Download the data from 2010 using the WDI() command
ind <- c(ind1,ind2,ind3)
dat <- WDI(indicator=ind,start=2010,end=2010)
# Store data values only and remove missing values
dat <- dat[,4:6]
dat <- dat[!apply(is.na(dat),1,any),]
colnames(dat)<-c("Health","GDP","Life")
head(dat)


dat$percentage_sepnd <- dat$Health / dat$GDP
dat$Health_log <- log(dat$Health)
head(dat[order(dat$percentage_sepnd, decreasing = TRUE),])



# 1 Compare different smoothing methods for finding the relationship between 'Health Exp' and 'GDP'

      # week 3/4
      # -------------------------------------------------
      # Lowess
      # - local regression
      # - different regression to predict each point
      # - uses a fraction of the data 'called the span' to predict a point
      # - 
library(MASS)
plot(dat$Health_log, dat$GDP, type='p', cex=.5)

fit <- lowess(dat$Health_log, dat$GDP,f=1/8)

points(fit,pch=3,col="red")

N <- 200
MSEarr <- rep(NA,N)
for (i in 1:N){
  fit <- lowess(dat$Health_log, dat$GDP,f=i/N)
  MSEarr[i] <- mean((dat$GDP-fit$y)^2)
  print(MSEarr[i])
}
plot(MSEarr, type='p', cex=.5)
# kernel smoothing

# Load the data
# Plot the data
plot(dat$Health_log, dat$GDP, type='p', cex=.5)
# Fit a kernel smoothing
# The bandwidth needs to be specified
fit <- ksmooth(dat$Health_log, dat$GDP,kernel="normal",bandwidth=1)
# Add the fitted values to the plot
points(fit,pch=3,col="red")
# Assess fit using mean squared error (MSE)
MSE <- mean((mcycle$accel-fit$y)^2)
MSE

N <- 100
MSEarr <- rep(NA,N)
for (i in 1:N){
  fit <- ksmooth(dat$Health_log, dat$GDP,kernel="normal",bandwidth=i/N)
  MSEarr[i] <- mean((dat$GDP-fit$y)^2)
  print(MSEarr[i])
}
plot(seq(0,N/N-1/N,by=1/N),MSEarr, type='p', cex=.5)


# smoothing splines, manually choosing knot location

#Load data and plot it
plot(dat$Health_log, dat$GDP, type='p', cex=.5)

# Load splines library
library(splines)
#Form B-spline basis functions for data
basis <- bs(dat$Health_log,knots=c(5,7))
# Fit the model
fit <- lm(dat$GDP~basis)
#Show fit on the plot
points(dat$Health_log,predict(fit),type="p",col="red")

# spline smoothing , auto choosing knot location
plot(dat$Health_log, dat$GDP, type='p', cex=.5)
fit.sp <- smooth.spline(dat$Health_log,dat$GDP,cv=TRUE)
points(dat$Health_log,predict(fit.sp,dat$Health_log)$y,col="green",type="p",lty=16)

#GAM
y <- dat$GDP
x <- dat$Health_log
fit.gam <- gam(y~s(x))
plot(dat$Health_log, dat$GDP, type='p', cex=.5)
points(x,predict(fit.gam,newdata=list(x=x)),col="purple",type="p",lty=2)


# 2 Does using a generalized additive model with GDP and Life Expectancy as covariates improve the model performance?
      # week 5
      # -------------------------------------------------
      # GAM
