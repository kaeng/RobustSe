# ============================================
# Part B)
# Multiple Regression setting, continuous predictors,
# variance as a function of predictor values
# ============================================

rm(list=ls())

set.seed(818)
x1 <- sort(rnorm(500,mean=30,sd=12))
x2 <- sort(rnorm(500,mean=50,sd=10))
r <- 1.5*x1 + 2*x2 - .01*x1*x2

y <- NA
for(i in 1:length(r)){
  stdev <- r^2/1000
  y[i] <- r[i] + rnorm(1,mean=0,sd=stdev[i])
}

summary(lm(y~x1+x2+x1*x2))

resids <- lm(y~x1+x2+x1*x2)$resid
fits <- lm(y~x1+x2+x1*x2)$fitted

plot(resids~fits)
