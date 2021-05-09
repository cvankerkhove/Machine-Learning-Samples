#File Name: Decision_Trees.R
#Description: This file contains a sample for construction of decision tree 
#methods, and corresponding progressions including random forest, and boosting
#trees. The tree package is used.
#Reference to the Textbook ISLR (James)
#Author(s): Chris VanKerkhove

setwd("~/Desktop/ML Samples")
library(dplyr)
library(tibble)
library(tree)
library(ISLR)

carseats <- as_tibble(Carseats) %>% mutate(High=as.factor(Sales > 8))

####Basic Tree Model ####
#making basic tree model
tree.carseats <- tree(High~.-Sales, carseats)
summary(tree.carseats)
#visualizing tree
plot(tree.carseats)
text(tree.carseats, pretty=0)
#training and test set
train <- sample(1:nrow(carseats), 200)
carseats.test <- carseats[-train,]
High.test <- carseats.test$High
tree.carseats <- tree(High~.-Sales, carseats, subset=train)
tree.pred <- predict(tree.carseats, carseats.test, type="class")
table(tree.pred, High.test)
#tree cross-validation using misclassifcation as pruning metric
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
#pruning based on cv
prune.carseats <- prune.misclass(tree.carseats, best=8)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred <- predict(prune.carseats, carseats.test, type="class")
table(tree.pred, High.test)


####Pruning Tree Through Cross-Validation####
set.seed(0)
##Decision tree 1
carseats.th <- select(carseats, -c(High))
train <- sample(1:nrow(carseats.th), nrow(carseats.th)/2)
tree1 <- tree(Sales~., carseats.th[train,])
plot(tree1)
text(tree1, pretty=0)
tree1.pred <- predict(tree1, carseats.th[-train,])
mse1 <- mean((carseats[train,]$Sales - tree1.pred)^2)
mse1
##Attempting to improve using cv and pruning
#tree cross-validation using misclassifcation as pruning metric
cv.1 <- cv.tree(tree1)
min(cv.1$dev)
#best size = 17
prune.tree1 <- prune.tree(tree1, best = 2)
plot(prune.tree1)
text(prune.tree1, pretty=0)
#prediction and mse from pruned tree
prune.tree1.pred <- predict(prune.tree1, carseats.th[-train,])
mse2 <- mean((carseats[train,]$Sales - prune.tree1.pred)^2)
mse2

####Using Random Forest with Decision  Trees####
library(randomForest)
##Use bagging and randomforest with these decision trees p=10
rf.tree1 <- randomForest(Sales~., data=carseats.th, mtry=10, importance=TRUE)
mean(rf.tree1$mse)
#comparing different values of mtry
mse3 <- c()
for (i in 1:10) {
  rf <- randomForest(Sales~., data=carseats.th, mtry=i, importance=TRUE)
  mse3[i] <- mean(rf$mse)
}
plot(1:10, mse3)


####Using Boosting Methods####
library(gbm)
##Use boosting 
#hitters dataset preprocess
hitters <- na.omit(Hitters)
hitters$Salary <- log(hitters$Salary)
#training split
train_ind <- sample(1:nrow(hitters), 180)
hitters.train <- hitters[train_ind,]
hitters.test <- hitters[-train_ind,]

##boosting over different lambdas
lambda <- 2^-(1:10)
test.mse <- c()
train.mse <- c()
for (i in 1:10) {
  bst <- gbm(Salary~., data=hitters.train, distribution="gaussian", n.trees=1000,shrinkage=lambda[i])
  bst.pred <- predict(bst, hitters.test, n.trees=1000, type="response")
  mse <- mean((hitters.test$Salary- bst.pred)^2)
  test.mse[i] <- mse
  train.mse[i] <- mean(bst$train.error)
}
#plotting lambdas and mse
plot(lambda, test.mse, main='Test MSE v Lambda')
plot(lambda, train.mse, main = 'Train MSE v Lambda')

#min test.mse
min(test.mse)
test.mse[5]

bst.hitters <- gbm(Salary~., data=hitters, distribution="gaussian", n.trees=1000,shrinkage= lambda[5])
summary(bst.hitters)
#mse of prediction
bst1.pred <- predict(bst.hitters, hitters, n.trees=1000, type="response")
mse4 <- mean((bst1.pred - hitters$Salary)^2)

