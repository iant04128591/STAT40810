# Fitting a multivariate model
# Generalized additive models via backfitting
# We use the same simulation as Handout 5.1

# Set seed for reproducibility
set.seed(1)

# Simulate data with two covariates x1 and x2)
# Store the resulting covariates in x
x1 <- runif(200,-1,1)
x2 <- runif(200,-1,1)
x <- cbind(x1,x2)

# The function that will link the covariates and y
f<-function(x)
{	
	x1<-x[1]
	x2<-x[2]
	cos(x1*pi)+sin(x2*2*pi)*exp(-x2^2)
}

# Simulate error terms e
# Let y=f(x)+e
e <- rnorm(200,0,1/4)
y <- apply(x,1,f)+e #apply function 'f' to each row of matrix 'x' and add 'e' to each of value

# Store the response variable as ynew
ynew <- y

### Start Loop

# Fit a spline smoothing with ynew as response
# and x1 as predictor
library(mgcv)

dat<-data.frame(x,ynew)
fit1 <- gam(ynew~s(x1),data=dat)

# Plot the fit
plot(fit1)

# Store the predictions from this model
pred1 <- predict(fit1,type="response")

# Compute the residuals of the model
res1 <- residuals(fit1)

# Compute mean of the squared residuals
mean(res1^2)

# Replace ynew be the difference between 
# the observed and predicted values 
ynew <- y - pred1

# Fit a spline smoothing with ynew as response
# and x2 as predictor

dat<-data.frame(x,ynew)
fit2 <- gam(ynew~s(x2))

# Plot the fit
plot(fit2)

# Store the predicted values from this model
pred2 <- predict(fit2,type="response",newdata=dat)

# Compute the residuals of the model
res2 <- residuals(fit2)

# Compute mean of the squared residuals
mean(res2^2)

# Replace ynew be the difference between 
# the observed and predicted values 
ynew <- y - pred2

### End Loop

# Do it all at once (using the gam() function)
dat<-data.frame(x,y)
fit <- gam(y~s(x1)+s(x2),data=dat)

# Look at the fit
par(mfrow=c(1,2))
plot(fit,select=1)
plot(x[,1],pred1)

plot(fit,select=2)
plot(x[,2],pred2)
par(mfrow=c(1,1))