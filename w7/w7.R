# Read in Failure Times
x <- c(79, 105, 14, 153, 67, 25, 39,9,55, 132,48, 570)
#Fit Kaplan-Meier Curve
# Also compute standard errors
library(survival)
fit <- survfit(Surv(x)~1,se=TRUE)
# Plot the Kaplan-Meier Curve with standard errors
plot(fit)


#Load survival package
library(survival)
#Load leukemia data
data(leukemia)
#Fit the model to the data
fit <- survfit(Surv(time,status)~x,data=leukemia,se=TRUE)
summary(fit)
# Plot the estimated survival curves
plot(fit,conf.int=FALSE,col=1:2)
# Plot with standard errors
plot(fit,conf.int=TRUE,col=1:2)