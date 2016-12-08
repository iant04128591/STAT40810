x<- rnorm(1000)
plot(x)

mean(x[1:200])
mean(x[201:400])
mean(x[401:600])
mean(x[601:800])
mean(x[801:1000])

sd(x[1:200])
sd(x[201:400])
sd(x[401:600])
sd(x[601:800])
sd(x[801:1000])


letters
x_cum <- cumsum(x)
plot(x_cum)

#from : https://www.r-bloggers.com/analysis-of-variance-anova-for-multiple-comparisons/
# first verify the homoskedasticity (ie test for homogeneity of variances).
groups <- factor(rep(letters[1:5], each = 200))
bt <- bartlett.test(x, groups)
ifelse(qchisq(0.950, as.double(bt$parameter)) > as.double(bt$statistic) , "H0: homoskedasticity variance", "H1: hetroskedasticity variance")
ft <- fligner.test(x, groups)
ifelse(qchisq(0.950, as.double(ft$parameter)) > as.double(ft$statistic) , "H0: homoskedasticity variance", "H1: hetroskedasticity variance")
fit = lm(formula = x ~ groups)
fit.anova <- anova(fit)
ifelse(qf(0.950, fit.anova$Df[2], fit.anova$Df[1]) > fit.anova$`F value`[1] , "H0: groups have equal mean", "H1: groups do not have equal mean")


mean(x_cum[1:200])
mean(x_cum[201:400])
mean(x_cum[401:600])
mean(x[601:800])
mean(x[801:1000])

sd(x_cum[1:200])
sd(x_cum[201:400])
sd(x_cum[401:600])
sd(x[601:800])
sd(x[801:1000])


groups <- factor(rep(letters[1:5], each = 200))
bt <- bartlett.test(x_cum, groups)
ifelse(qchisq(0.950, as.double(bt$parameter)) > as.double(bt$statistic) , "H0: homoskedasticity variance", "H1: hetroskedasticity variance")
ft <- fligner.test(x_cum, groups)
ifelse(qchisq(0.950, as.double(ft$parameter)) > as.double(ft$statistic) , "H0: homoskedasticity variance", "H1: hetroskedasticity variance")
fit = lm(formula = x_cum ~ groups)
fit.anova <- anova(fit)
ifelse(qf(0.950, fit.anova$Df[2], fit.anova$Df[1]) > fit.anova$`F value`[1] , "H0: groups have equal mean", "H1: groups do not have equal mean")
