#File Name: validation.R
#Description: This file contains a sample for standard validation techniques to 
#a model, including validation setl LOOCV, K-fold CV, and bootstrap
#Author(s): Chris VanKerkhove
setwd("~/Desktop/ML Samples")
library(tibble)
library(ISLR)
library(boot)
#Data
corollas <- as_tibble(read.csv("Data/ToyotaCorolla.csv", header = T))
sum(is.na(corollas))
set.seed(1)
####The Validation Set Approach#####
#split into train / validation
train_ind <- sample(nrow(corollas), nrow(corollas) / 2)
lm.fit = lm(Price~Age_08_04+KM, data=corollas, subset=train_ind)
mean(((corollas$Price - predict(lm.fit, corollas))[-train_ind])^2)

#evaluation function and degree check
ptm = proc.time()
eval_lm <- function(deg) {
  lm.fit <- lm(Price~poly(Age_08_04, deg)+poly(KM,deg),
               data=corollas, subset=train_ind)
  return(mean(((corollas$Price - predict(lm.fit, corollas))[-train_ind])^2))
  
}
vset = proc.time() - ptm
print('MES on degrees 1 to 5')
print(sapply(1:5, eval_lm))

####Leave one out Cross-Validation (LOOCV)####
ptm = proc.time()
loocv.error <- rep(0.0,5) # 5-dim. vector of zeros
for (deg in 1:5) {
  glm.fit <- glm(Price~poly(Age_08_04, deg)+poly(KM, deg), data=corollas)
  loocv.error[deg] <- cv.glm(corollas, glm.fit)$delta[1]
}
loocv_t = proc.time() - ptm
#plot
# pdf("locv_error.pdf", 7, 5)
plot(loocv.error, type="b")

#Get summary of p-values for quadratic and cubic modles
glm.fit_2 <- glm(Price~poly(Age_08_04, 2)+poly(KM,2), data=corollas)
glm.fit_3 <- glm(Price~poly(Age_08_04, 3)+poly(KM, 3), data=corollas)
print(summary(glm.fit_2))
print(summary(glm.fit_3))


####K-Fold Cross-Validation####
ptm = proc.time()
cv.error <- matrix(rep(0,10), ncol=2)
for (deg in 1:5) {
  glm.fit <- glm(Price~poly(Age_08_04, deg)+poly(KM,deg), data=corollas)
  cv.error[deg,1] <- cv.glm(corollas, glm.fit, K=5)$delta[1]
  cv.error[deg,2] <- cv.glm(corollas, glm.fit, K=10)$delta[1]
  
}
k_fold_t = proc.time() - ptm
#pdf("k-fold_error.pdf", 7, 5)
plot(c(1:5), cv.error[,1], lty=1, col="blue", type="b")
lines(c(1:5), cv.error[,2], lty=2, col="green", type="b")
legend("topright", c("K=5", "K=10"), col=c("blue", "green"), lty=1:2)
par(mfrow = c(1,1))

#run-time comparison
#validation set
vset
loocv_t
k_fold_t

####Bootstrapping####
#(with Logistic Regression)
library(tidyr)
library(dplyr)
set.seed(1)
n = 1000
x1 <- runif(n)
x2 <- runif(n, -2, 1)
z <- (x1-0.2)*(x1-0.5)*(x1-0.9) * 25 - x2*(x2+1.2)*(x2-0.8) + rnorm(n)/3
y <- as.integer(z>0)
plot(x1, x2, col=c("red", "blue")[y+1])
df <- data.frame(x1,x2,y)
boot_fn <- function(data, train_id){
  "Function that serves as the function for the 'statistic' parameter of the 
  'boot()' function. The statistic this function returns is the error of this 
  bootstrapped dataset
  Note: (data and train_id are parameters provided by the 'boot()' function)
  "
  logit <- glm(y~., data = df[train_id,], family = binomial)
  pred <- predict(logit, df, type = "response")
  pred_bin <- as.numeric(pred >= 0.5)
  error <- sum(as.numeric(pred_bin != df$y)) / length(df$y)
  return (error)
}

bs_df <- boot(df, boot_fn, R=1000)
bs_df$statistic()
