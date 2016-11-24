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
y <- apply(x,1,f)+e

# Plot the function f(x) on the range [-1,1]x[-1,1]
x1g <- seq(-1,1,length=51)
x2g <- seq(-1,1,length=51)
xg <- expand.grid(x1g,x2g)
colnames(xg)<-c("x1","x2")
fg <- apply(xg,1,f)
fg <- matrix(fg,51,51)

# Produce a 3D plot
persp(x1g,x2g,fg,col="lightblue")

# Produce a contour plot
contour(x1g,x2g,fg)

# Use the rgl library to produce an interactive plot
#sudo apt-get install libglu1-mesa-dev
install.packages('rgl')
library(rgl)

# Plot the function f(x)
persp3d(x1g,x2g,fg,col="lightblue")

# Add the data points (x1,x2,y)
plot3d(x1,x2,y,add=TRUE)

# Automatically spin the plot
play3d(spin3d(axis = c(0, 0, 1), rpm = 16), duration = 2.5)

# Fit a generalized additive model
library(mgcv)
fit <- gam(y~s(x1)+s(x2))
pred <- predict(fit)

# Add the predicted y values (x1,x2,pred)
predg <- predict(fit,newdata=xg)
predg <- matrix(predg,51,51)

# Plot the estimated f(x)
persp3d(x1g,x2g,fg,col="lightblue",front="fill",back="fill")
plot3d(x1,x2,y,add=TRUE)
persp3d(x1g,x2g,predg,col="lightgreen",add=TRUE,front="lines",back="lines")

# Automatically spin the plot
play3d(spin3d(axis = c(0, 0, 1), rpm = 16), duration = 2.5)