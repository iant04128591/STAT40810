# Generate a set of x values
# Sort the x values (this eases later analysis)
set.seed(1)
x <- rnorm(100)
x <- sort(x)

# Generate values of f(x) for some function
f <- function(x)
{
	sin(pi*x)
}

# Generate some residuals
e <- rnorm(100,0,1/4)

# Generate y values which are y=f(x)+e
y <- f(x)+e

# Plot the data
plot(x,y,pch=3)

# Add the "true" curve to the plot
xs<-seq(min(x),max(x),length=101)
points(xs,f(xs),type="l",col="red")

# Open mgcv package and splines package
# Check you have it installed
library(mgcv)

# Set up a vector of values for the tuning parameter
# Set up a vector to store the results
##lambda
svec <- seq(0,1,length=51) 

MSEvec <- rep(NA, 51)    

for (i in 1:51)
{
	# Fit smoothing spline
	fit.sp <- smooth.spline(x,y,spar=svec[i])

	# Plot the fit
	points(x,predict(fit.sp,x)$y,col="blue",type="l",lty=4)

	# Least squares criterion
	MSEvec[i] <- mean((predict(fit.sp,x)$y-y)^2)
}

plot(svec,MSEvec,xlab="Smoothing parameter",ylab="MSE",log="y",pch=3)

# Let's look at the "best" fit
soptimal <- svec[MSEvec==min(MSEvec)]

# Fit smoothing spline
fit.sp <- smooth.spline(x,y,spar=soptimal)

# Plot the fit
plot(x,y,pch=3)
points(xs,predict(fit.sp,xs)$y,col="blue",type="l",lty=4)

# Add the "true" curve
points(xs,f(xs),type="l",col="red")
