#File Name: GAMs.R
#Description: This file contains a sample for construction of certain GAMs 
#(Generalized Additive Models), for training models on data that is pressumed 
#to be nonlinear. ANOVA method for errors is also demonstrated. Finally, 
#backfitting with multiple linear regression is demonstrated
#Reference to the Textbook ISLR (James)
#Author(s): Chris VanKerkhove

setwd("~/Desktop/ML Samples")
library(ISLR)
library(boot)
library(splines)
library(gam)

####-Generalized Additive Models
set.seed(1)
#allows access to these variables
attach(Wage)
#GAM 1 (natural splines)
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
#GAM 2(smoothing splines)
gam2 <- gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(2,3))
plot.Gam(gam1, se=TRUE, col="blue")
plot(gam2, se=TRUE, col="red")

#compare GAMs via ANOVA
gam.m1 <- gam(wage~s(age,5) + education, data=Wage)
gam.m2 <- gam(wage~year + s(age,5) + education, data=Wage)
gam.m3 <- gam(wage~s(year, 4) + s(age, 5) + education, data=Wage)
gamTest <- anova(gam.m1, gam.m2, gam.m3, test="F")
print(gamTest)

#logistic regression with GAMs
gam.logreg <- gam(I(wage>250)~ year + s(age,df=5) + education, family=binomial, 
                  data=Wage)
par(mfrow=c(1,3))
plot(gam.logreg, se=T, col='blue')
#inspecting data
levels(education[I(wage >250)])
table(education, I(wage>250))
#fixing gam.logreg
gam.fix <- gam(I(wage>250)~year + s(age, df=5) + education, family=binomial, 
               data=Wage, subset=(education != "1. < HS Grad"))
par(mfrow=c(1,3))
plot(gam.fix, se=T, col="blue")

####- Backfitting with multiple linear regression
set.seed(1)
X1 = rnorm(100)
X2 = rnorm(100)
X3 = sin(X2)
eps = rnorm(100, sd = 0.01)
Y=10+0.8 *X1+6*X2+ 1.1*X3+eps
df <- data.frame(Y,X1, X2, X3)
#initalize B1
b1 <- 3
#iteration 1
lmFix1 <- lm(Y - b1 * X1 ~ ., data=df)
summary(lmFix1)
b2 <- lmFix1$coefficients['X2']
print(b2)
#iteration 2
lmFix2 <- lm(Y - b2 * X2 ~ ., data=df)
summary(lmFix2)
print(lmFix2$coefficients['(Intercept)'])
print(lmFix2$coefficients['X3'])
b3 <- lmFix2$coefficients['X3']
#iteration 3
lmFix3 <- lm(Y - b3 * X3 ~., data=df)
summary(lmFix3)
b1 <- lmFix3$coefficients['X1']
lmFix3$coefficients
#part vi, loop for iterations
coef.iter <- function(n, v, plt = FALSE){
  #backfitting with multiple linear regression
  #n: number of iterations
  #v: starting value for B1
  B1.list <- rep(NA, n)
  B2.list <- rep(NA, n)
  B3.list <- rep(NA, n)
  B0.list <- rep(NA, n)
  #starting value
  b1 = v
  for (i in 1:n){
    #fixing B1
    lmFix1 <- lm(Y - b1 * X1 ~ ., data=df)
    b2 <- lmFix1$coefficients['X2']
    #fixing B2
    lmFix2 <- lm(Y - b2 * X2 ~ ., data=df)
    b3 <- lmFix2$coefficients['X3']
    #fixing b3
    lmFix3 <- lm(Y - b3 * X3 ~., data=df)
    b1 <- lmFix3$coefficients['X1']
    B1.list[i] <- b1
    B2.list[i] <- b2
    B3.list[i] <- b3
    B0.list[i] <- lmFix3$coefficients['(Intercept)']
  }
  if (plt) {
    par(mfrow=c(2,2))
    plot(1:n,B1.list)
    plot(1:n, B2.list)
    plot(1:n, B3.list)
    plot(1:n, B0.list)
  }
  #returning final values
  return (c(b1,b2,b3))
}
backfit <- coef.iter(10, 3, plt = TRUE)
print(backfit)

