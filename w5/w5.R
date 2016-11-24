# Read in the data
# (you might need to set the working directory)
bankbill <- read.table("bankbill.txt",header=TRUE)

head(bankbill)
# Fit a generalized additive model
fit0 <- gam(BankBill~s(Bank)+s(Unemploy)+s(CPI),data=bankbill)
summary(fit0)
# Add interaction term between CPI and Unemploy
fit1 <- gam(BankBill~s(Bank)+s(Unemploy)+s(CPI)+s(CPI,Unemploy),data=bankbill)
summary(fit1)
# Compare the models
anova(fit0,fit1)


# Load the rock data
data(rock)
# Produce a pairs plot of the data
pairs(rock)
# Add a column with log(perm)
rock$lperm <- log(rock$perm)
# Fit a PPR model
fit <- ppr(lperm~area+peri+shape,data=rock,nterms=2)
# Examine the fit
summary(fit)
# Plot the smooth functions used in each term
par(mfrow=c(1,2))
plot(fit)
par(mfrow=c(1,1))