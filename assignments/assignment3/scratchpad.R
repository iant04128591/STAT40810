install.packages('emplik')

library(emplik)
library(survival)

data(myeloma)

head(myeloma)


###############################
# regression analysis recap

##############################
x <- c(14, 13, 13, 14, 14, 12, 12, 15, 13, 12, 11, 14, 12, 15, 16, 12, 15, 11, 15)
y <- c(69, 56.5, 65.3, 62.8, 63.5, 57.3, 59.8, 62.5, 62.5, 59.0, 51.3, 64.3, 56.3, 66.5, 72.0, 64.8, 67.0, 57.5, 66.5)
df <- data.frame(x,y) 
library(ggplot2)
ggplot(data=df, aes(x,y)) + geom_point() + geom_smooth(method='lm')
model <- lm(y~x)
#reject null hypothesis that alpha = 0 / beta = 0 as p-values are less than 0.05
#Hence there is a significant relationship between the variables in the linear regression model of the data set faithful.
summary(model)
model.predict <- predict(model, interval="confidence") 
newx <- seq(min(df$x), max(df$x), length.out=100)
preds <- predict(model, newdata = data.frame(x=newx), interval = 'confidence')
#plot
plot(x,y)
#model
abline(model)
# intervals
#upper
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
#lower
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = rgb(1,0,0,0.25), border = NA)
###############################


##Question 1
library(emplik)
library(survival)
data(myeloma)
colnames(myeloma) <- c("time","vstatus","logBUN","HGB","platelet","age","logWBC","FRAC","logPBM","protein","SCALC")
myeloma <- as.data.frame(myeloma)
head(myeloma)
str(?myeloma)
fit <- coxph(Surv(time,vstatus) ~ logBUN + HGB:platelet, data=myeloma)
plot(survfit(fit), lty=c(1,3,3), ylab = "Survival Probability", xlab = "Months", bty="L",col = c("blue", "green","red"))
legend(70,0.9,c("Survival Prediction","Lower","Upper"), lty=c(1,3,3), bty = "n",col = c("blue", "green","red")) 
print(fit)
summary(fit)

install.packages('survminer')
library(survminer)
ggsurvplot(survfit(fit), risk.table = TRUE)

predict(fit,data.frame(myeloma))

install.packages('survMisc')
library(survMisc)
survMisc:::autoplot.survfit(fit)

library(survival)
ggkm(survfit(fit))



##Question 2
x <- c(4, 13, 3, 6, 4, 3, 1, 1, 10, 6, 6, 20, 3, 15)
x.cencored <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1)
N <- length(x)
dat <- data.frame(time=x,status=x.cencored)
survdat <- Surv(time=dat[,1],event=dat[,2])
survdat
dat.fit <- survfit(survdat~1,se=TRUE)
summary(dat.fit)

plot(dat.fit, lty=c(1,3,3), ylab = "Survival Probability", xlab = "Weeks", bty="L",col = c("blue", "green","red"))
legend(12,1,c("Survival Prediction","Lower","Upper"), lty=c(1,3,3), bty = "n",col = c("blue", "green","red")) 



plot(stepfun(1:7,cumprod(1-dat.fit$n.event/dat.fit$n.risk)))
lines(stepfun(1:7,km.se + cumprod(1-dat.fit$n.event/dat.fit$n.risk)), col='black', lty=3)

lines(stepfun(1:7,cumprod(exp(-dat.fit$n.event/dat.fit$n.risk))), col='red')


km.se <- sqrt(sum(dat.fit$n.event/(dat.fit$n.risk*(dat.fit$n.risk-dat.fit$n.event))))

na.se <- exp(sum(-dat.fit$n.event/(dat.fit$n.risk^2)))

#Load survival package
library(survival)
#Load leukemia data
str(leukemia)
data(leukemia)
#Extract data for Maintained group
dat<-leukemia[leukemia$x=="Maintained",1:2]
dat
#Set up Surv() object
survdat <- Surv(time=dat[,1],event=dat[,2])
survdat
#Fit the KM to the data
fit <- survfit(survdat~1,se=FALSE)
summary(fit)
plot(fit)