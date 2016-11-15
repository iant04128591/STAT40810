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
fit <- gam(lperm~s(area)+s(peri)+s(shape),data=rock)

par(mfrow=c(2,2))
plot.gam(fit,scale=0,se=FALSE)
par(mfrow=c(1,1))

# Re-fit the generalized additive model with
# area and peri being linear and only shape 
# being transformed
fit1 <- gam (lperm~area+peri+s(shape),data=rock)

plot.gam(fit1)
