#loess
library(MASS)
library(ggplot2)
data(mcycle)


mcycle %>% ggplot(data = .,aes(x = times, y = accel)) + geom_point() + geom_smooth(method='lm')
mcycle %>% ggplot(data = .,aes(x = times, y = accel)) + geom_point() + geom_smooth(method='glm')
mcycle %>% ggplot(data = .,aes(x = times, y = accel)) + geom_point() + geom_smooth(method='gam')
mcycle %>% ggplot(data = .,aes(x = times, y = accel)) + geom_point() + geom_smooth(method='rlm')

mcycle %>% ggplot(data = .,aes(x = times, y = accel)) + geom_point() + geom_smooth(method='loess', span=0.)
mcycle %>% 
  ggplot(data = .,aes(x = times, y = accel)) + 
  geom_point() + 
  geom_smooth(method='loess', span=0.25, color='red', size=.25) + 
  geom_smooth(method='loess', span=0.75, size=.25)


fit.lowess <- lowess(x = mcycle$times, y = mcycle$accel, f=2/3)

plot(mcycle)
lines(fit, pch=3,col='red')
MSE <- mean((mcycle$accel-fit$y)^2)
MSE



fit.ksmooth.normal <- ksmooth(x = mcycle$times, y = mcycle$accel, kernel = 'normal', bandwidth = 0.5)
lines(fit.ksmooth.normal, pch=5,col='blue')
MSE <- mean((mcycle$accel-fit.ksmooth.normal$y)^2, na.rm = TRUE)
MSE
fit.ksmooth.normal <- ksmooth(x = mcycle$times, y = mcycle$accel, kernel = 'normal', bandwidth = 5)
MSE <- mean((mcycle$accel-fit.ksmooth.normal$y)^2, na.rm = TRUE)
MSE
lines(fit.ksmooth.normal, pch=5,col='green')
fit.ksmooth.box <- ksmooth(x = mcycle$times, y = mcycle$accel, kernel = 'box', bandwidth = 2)
MSE <- mean((mcycle$accel-fit.ksmooth.box$y)^2, na.rm = TRUE)
MSE
lines(fit.ksmooth.box, pch=12,col='orange')


library(splines)

basis <- bs(mcycle$times, knots = c(13,22,30,40))
fit.spline <- lm(mcycle$accel ~ basis)
points(x = mcycle$times, y = predict(fit.spline), col='red')





# Load the rock data
data(rock)
# Produce a pairs plot of the data
pairs(rock)
# Add a column with log(perm)
rock$lperm <- log(rock$perm)
# Fit a multiple linear regression model
fit0 <- lm(lperm~area+peri+shape,data=rock)
# Fit a generalized additive model
library(mgcv)

fit <- gam(lperm~s(area)+s(peri)+s(shape),data=rock, control = gam.control(epsilon=1e-20,trace = TRUE))
par(mfrow=c(2,2))
plot.gam(fit,scale=0,se=FALSE)
par(mfrow=c(1,1))
# Re-fit the generalized additive model with
# area and peri being linear and only shape
# being transformed
fit1 <- gam (lperm~area+peri+s(shape),data=rock)
plot.gam(fit1)