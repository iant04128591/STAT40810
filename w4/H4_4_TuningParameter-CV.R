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
svec <- seq(0,1,length=51)
N<-length(x)
MSEmat <- matrix(NA,51,N)

for (i in 1:51)
{
	for (n in 1:N)
	{
		# Fit smoothing spline with observation n dropped
		fit.sp <- smooth.spline(x[-n],y[-n],spar=svec[i])

		# Least squares criterion (for this fit) for observation n
		MSEmat[i,n] <- (predict(fit.sp,x[n])$y-y[n])^2
	}
}

MSEvec <- apply(MSEmat,1,mean)

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

# Much easier way...
fit.sp <- smooth.spline(x,y,cv=TRUE)
points(xs,predict(fit.sp,xs)$y,col="green",type="l",lty=1)

# And another way... (but slightly different)
fit.gam <- gam(y~s(x))
points(xs,predict(fit.gam,newdata=list(x=xs)),col="purple",type="l",lty=2)
