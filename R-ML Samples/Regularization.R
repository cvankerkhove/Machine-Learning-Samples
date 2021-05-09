#File Name: Regularization.R
#Description: This file contains a sample for different regularization methods
#such as simple subset selection of predictors, Ridge Regression, and Lasso
#(L1 & L2 regularization)
#Reference to the Textbook ISLR (James)
#Author(s): Chris VanKerkhove

setwd("~/Desktop/ML Samples")
library(tibble)
library(dplyr)
library(boot)
library(ISLR)
library(glmnet)
library(leaps)

#data pre-processing
df <- as_tibble(na.omit(Hitters))
model.pre <- lm(Salary~., df)
summary(model.pre)$adj.r.squared
outliers <- abs(rstudent(model.pre)) > 3
df.post <- df[!outliers,]
model.proc <- lm(Salary~., df.post)
summary(model.proc)$adj.r.squared

#trying no intercept model
model.noint <- lm(Salary~. + 0, df.post)
summary(model.noint)$adj.r.squared

####Subset Selection Methods####
#best subset selection, no intercept
regfit.full <- regsubsets(Salary~., data=df.post, nvmax=19, intercept=F)
reg.summary <- summary(regfit.full)

#print Cp, BIC, R^2
#pdf("cp.pdf")
plot(reg.summary$cp, xlab="p", ylab=expression(C[p]), type="b")
dev.off()
#pdf("bic.pdf")
plot(reg.summary$bic, xlab="p", ylab="BIC", type="b")
#pdf("adj_r2.pdf")
plot(reg.summary$adjr2, xlab='p', ylab=expression(R^2), type="b")


####Ridge and Lasso####
x <- model.matrix(Salary~., df.post)[,-1]
y <- df.post$Salary

grid <- 10^seq(10, -2, length=100)

ridge.mod <- glmnet(x,y, alpha=0, lambda=grid, intercept=FALSE)
l2norm <- sqrt(apply(ridge.mod$beta^2, 2, sum))
#pdf("ridge_l2.pdf)
plot(log(grid), l2norm)

#Split data into training data set and test data
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
y.train <- y[train]
y.test <- y[-train]

##RIDGE
#model fit on the training set
ridge.mod <- glmnet(x[train,], y.train, alpha=0, lambda=grid, intercept=FALSE)
set.seed(1)
cv.ridge <- cv.glmnet(x[train,], y.train, nfolds = 10, lambda=grid, alpha=0, intercept=FALSE)
lbest <- cv.ridge$lambda.min
ridge.pred <- predict(ridge.mod, s=lbest, newx=x[-train,])
ridge.mse <- mean((ridge.pred - y.test)^2)
print(paste("Ridge MSE (Hitters): ", ridge.mse))
print(paste("Optimal lambda (Hitters): ", lbest))
plot(cv.ridge)

##LASSO
#model fit on training set
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid, intercept = FALSE)
#Use 10-fold cross-validation to choose lambda
set.seed(1)
cv.lasso <- cv.glmnet(x[train,], y[train], alpha=1, lambda=grid, intercept=FALSE)
lbest <- cv.lasso$lambda.min
lasso.pred <- predict(lasso.mod, s=lbest, newx=x[-train,])
lasso.mse <- mean((lasso.pred - y.test)^2)
print(paste("LASSO MSE (Hitters: ", lbest))
print(paste("Optimal lambda (hitters): ", lasso.mse))
plot(cv.lasso)

