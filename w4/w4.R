rm(list = ls())


#Load the cars data
data(cars)
# Plot the data
plot(cars,pch=3)
# Fit the model using CV
fit1 <- smooth.spline(cars$speed,cars$dist,cv=TRUE)
points(predict(fit1),type="l",col="red")
#Fit the model using GCV
fit2 <- smooth.spline(cars$speed,cars$dist,cv=FALSE)
points(predict(fit2),type="l",col="purple",lty=2)
# An alternative function for GCV
library(mgcv)
fit3 <- gam(dist~s(speed),data=cars)
points(cars$speed,predict(fit3),type="l",col="blue",lty=3)
# Assess fit
mean(residuals(fit1)^2)
mean(residuals(fit2)^2)

#Load the motorcycle data
library("MASS")
data(mcycle)
# Plot the data
plot(mcycle,pch=3)
# Fit the model using CV
fit1 <- smooth.spline(mcycle$times,mcycle$accel,cv=TRUE)
points(predict(fit1),type="l",col="red")
#Fit the model using GCV
fit2 <- smooth.spline(mcycle$times,mcycle$accel,cv=FALSE)
points(predict(fit1),type="l",col="purple",lty=2)
# Assess fit
mean(residuals(fit1)^2)
mean(residuals(fit2)^2)


#Load the cholostyramine data
library("bootstrap")
data(cholost)
help(cholost)
# Plot the data
plot(cholost,pch=3)
# Fit the model using CV
fit1 <- smooth.spline(cholost$z,cholost$y,cv=TRUE)
points(predict(fit1),type="l",col="red")
#Fit the model using GCV
fit2 <- smooth.spline(cholost$z,cholost$y,cv=FALSE)
points(predict(fit2),type="l",col="purple",lty=2)
# Assess fit
mean(residuals(fit1)^2)
mean(residuals(fit2)^2)
