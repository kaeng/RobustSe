# ============================================
# Part A)
# Discrete preditors / ANOVA setting
# with non-constant variance
# ============================================

# ============================================
# Setting up data for a single simulation
# ============================================

rm(list=ls())

set.seed(818)

FactorA <- rep(seq(1,2),each=20)
FactorB <- rep(rep(c(1,2),each=10),2)
r <- rep(c(140,69,86,127),each=10) #previously, runif(4,50,150)
# (mu11, mu12, mu21, mu22)

alpha2 = 

# calculate effect sizes (alphas, betas, alphabetas)


# standard deviations within 4 groups
y <- NA
for(i in 1:length(r)){
  stdev <- rep(seq(10,20,length=4),each=10)
  y[i] <- r[i] + rnorm(1,mean=0,sd=stdev[i])
}

anovadata <- as.data.frame(cbind(FactorA, FactorB, y))

head(anovadata)
anovadata$FactorA <- as.factor(FactorA)
anovadata$FactorB <- as.factor(FactorB)
str(anovadata)

#checking out the data.
library("ggplot2")
  ggplot(anovadata, aes(FactorA, y, colour=FactorB)) +
  geom_boxplot(outlier.colour=NULL) +
  stat_summary(aes(group=FactorB), fun.y = mean, geom="point", size=3.5) +
  stat_summary(aes(group=FactorB), fun.y = mean, geom="line") +
  scale_x_discrete("FactorA") +
  scale_y_continuous("y") +
  theme_bw()

#fitting model using OLS

mod <- lm(y~FactorA*FactorB,anovadata) #full model
summary(mod)
anova(mod)
class(mod$coeff)

