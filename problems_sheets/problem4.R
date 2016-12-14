#Load survival package
library(survival)
#Load leukemia data
data(leukemia)
#Fit the Cox PH model to the data
fit <- coxph(Surv(time,status)~x,data=leukemia)
summary(fit)
predict(fit)


beta <- fit$coefficients

exp(beta*leukemia$time)

data(lung)
head(lung)
str(lung)

fit <- coxph(Surv(time,status)~age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss,data=lung)


lung$sex = lung$sex -1 
lung$sex = lung$sex %% 2 