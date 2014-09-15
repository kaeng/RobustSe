# ============================================
# Part B)
# Model Mispecification
# missing important predictors (x2 and interaction)
# ============================================

rm(list=ls())

set.seed(818)
x1 <- sort(rnorm(500,mean=30,sd=12))
x2 <- sort(rnorm(500,mean=50,sd=10))
r <- 1.5*x1 + 2*x2 - .01*x1*x2

y <- NA
for(i in 1:length(r)){
  y[i] <- r[i] + rnorm(1,mean=0,sd=rnorm(1,0,20))
}

summary(lm(y~x1))

resids <- lm(y~x1)$resid
fits <- lm(y~x1)$fitted

plot(resids~fits)