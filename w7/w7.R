rm(list=ls())
# Read in Failure Times
x <- scan()
79 105 14 153 67 25 39
9 55 132 48 570

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



#Load survival package
library(survival)
#Load leukemia data
data(leukemia)
#Fit the Cox PH model to the data
fit <- coxph(Surv(time,status)~x,data=leukemia)
summary(fit)


#Load survival package
library(survival)
#Load the GBSG2 data
install.packages('TH.data')
library(TH.data)
data("GBSG2")
GBSG2
# Change tumor grade to unordered for interpretation
GBSG2$tgrade <- factor(GBSG2$tgrade,ordered=FALSE)
#Fit a Cox proportional hazards model
fit <- coxph(formula = Surv(time, cens) ~ ., data = GBSG2)
# Examine the fit
print(fit)
summary(fit)