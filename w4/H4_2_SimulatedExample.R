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

# Plot the data
plot(x,y,pch=3)

# Add the "true" curve to the plot
xs<-seq(min(x),max(x),length=101)
points(xs,f(xs),type="l",col="red")

# Open mgcv package and splines package
# Check you have it installed
library(mgcv)

# Fit smoothing spline
# spar parameter = 'lambda'
fit.sp <- smooth.spline(x,y,spar=1)

# Plot the fit
points(x,predict(fit.sp,x)$y,col="blue",type="l",lty=4)

# Least squares criterion
mean(residuals(fit.sp)^2)
mean((predict(fit.sp)$y-y)^2)
