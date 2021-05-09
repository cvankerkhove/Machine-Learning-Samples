#File Name: KNN.R
#Description: This file contains a sample for construction a KNN classification
#model and checking accuracy through an accuracy table
#Reference to the Textbook ISLR (James)
#Author(s): Chris VanKerkhove
setwd("~/Desktop/ML Samples")
library(readr)
library(tibble)
library(dplyr)

####K Nearest Neighbors####
####Data Cleaning and Manipulation###
titanic <- read_csv("Data/titanic.csv") %>%
  tibble()
#replace with mean
titanicreplc <- titanic2
titanicreplc$Age[is.na(titanic$Age)] = median(titanic2$Age)
#remove NAs
titanic_model <- titanic2[complete.cases(titanic2),]
#checking for dupicated again
sum(duplicated(titanic_model))

library("class")
sapply(titanic_model, class)
titanic_model$Sex <- model.matrix(~ Sex - 1, data=titanic_model)
titanic_model$Embarked <- model.matrix(~ Embarked - 1, data=titanic_model)
sapply(titanic_model, class)
#Generate training data set and test data set
set.seed(1)
train_ind <- sample(1:nrow(titanic_model), 2/3*nrow(titanic_model))
#Normalization
titanic_normal <- scale(titanic_model[, !names(titanic_model) %in% "Survived"])
titanic.train <- titanic_normal[train_ind,]
titanic.test <- titanic_normal[-train_ind,]
#Store the outcome column separately
train.survive=titanic_model$Survived[train_ind]
test.survive=titanic_model$Survived[-train_ind]

####Model Construction and Prediction####
#Implement the KNN algorithm
knn.pred=knn(titanic.train, titanic.test, train.survive, k=1)
knn.pred2=knn(titanic.train, titanic.train, train.survive, k=1)
table(knn.pred, test.survive)
1-mean(knn.pred==test.survive)
error <- rep(0,6)
K <- c(1,3,5,10,20,50)
for (i in (1:length(K))){
  set.seed(1)
  knn.pred=knn(titanic.train,titanic.test, train.survive,k=K[i]);
  error[i] <- 1-mean(knn.pred==test.survive)
}
error

####Running Alternative Methods to Compare Accuracy####
K <- seq(from=1, to=20, by=1)
error <- rep(0, length(K))
for (i in (1:length(K))){
  set.seed(1)
  knn.pred=knn(titanic.train,titanic.test, train.survive,k=K[i]);
  error[i] <- 1-mean(knn.pred==test.survive)
}
#plotting with normalization
plot(K, error)

####without normalization
train_ind <- sample(1:nrow(titanic_model), 2/3*nrow(titanic_model))
#Normalization
titanic.train <- titanic_model[train_ind,]
titanic.test <- titanic_model[-train_ind,]
#Store the outcome column separately
train.survive=titanic_model$Survived[train_ind]
test.survive=titanic_model$Survived[-train_ind]
#Implement the KNN algorithm
knn.pred=knn(titanic.train, titanic.test, train.survive, k=1)
K <- seq(from=1, to=20, by=1)
error <- rep(0, length(K))
for (i in (1:length(K))){
  set.seed(1)
  knn.pred=knn(titanic.train,titanic.test, train.survive,k=K[i]);
  error[i] <- 1-mean(knn.pred==test.survive)
}
plot(K,error)

###Doubling the training set
#non-doubled KNN
knn.pred=knn(titanic.train, titanic.test, train.survive, k=5)
error.nondub <- 1-mean(knn.pred==test.survive)
#doubled data
titanic.train2 <- bind_rows(as.data.frame(titanic.train), as.data.frame(titanic.train))
knn.pred=knn(titanic.train2, titanic.test, append(train.survive, train.survive), k=5)
error.dub <- 1-mean(knn.pred==test.survive)
###Checking for 2k with multiple k values
K <- seq(from=1, to=30, by=1)
K2 <- K*2
error.nondub <- rep(0, length(K))
error.dub <- rep(0, length(K))
for (i in (1:length(K))){
  set.seed(1)
  knn.pred=knn(titanic.train,titanic.test, train.survive,k=K[i], use.all = FALSE);
  knn.pred2 = knn(titanic.train2, titanic.test, append(train.survive, train.survive), k = 2*K[i], use.all = FALSE)
  error.nondub[i] <- 1-mean(knn.pred==test.survive)
  error.dub[i] <- 1-mean(knn.pred2==test.survive)
}
#plotting 
plot(K, error.nondub)
plot(K2, error.dub)


