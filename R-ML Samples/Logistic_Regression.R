#File Name: Logistic_Regression.R
#Description: This file contains a sample for construction a logistic regression
#model and checking accuracy through an accuracy table
#Author(s): Chris VanKerkhove

setwd("~/Desktop/ML Samples")
library(readr)
library(tibble)
library(dplyr)

#####Logistic Regression#####
####Example 1#####
###Data Cleaning and Manipulation###
titanic <- read_csv("Data/titanic.csv") %>%
  tibble()
#duplicated values
n <- sum(duplicated(titanic))
titanic2 <- select(titanic, -c("PassengerId", "Name", "Ticket", "Cabin" ))
#missing values
missing <- c()
for (col.name in colnames(titanic2)) {
  missing[col.name] <- sum(is.na(titanic2[[col.name]]))
}
view(missing)
#replace with mean
titanicreplc <- titanic2
titanicreplc$Age[is.na(titanic$Age)] = median(titanic2$Age)
#remove NAs
titanic_model <- titanic2[complete.cases(titanic2),]
#checking for dupicated again
sum(duplicated(titanic_model))


###Model Construction Operations (for the removing NAs data)
#training and testing index in data
train_ind <- sample(1:nrow(titanic_model), 2/3*nrow(titanic_model))
titanic_train <- titanic_model[train_ind,]                    
titanic_test <- titanic_model[-train_ind,]

#creating the model
titanic_logit <- glm(Survived~., data = titanic_train, family = binomial)
#running test data through model
titanic_pred <- predict(titanic_logit, titanic_test, type = "response")
#summary of fitted model
summary(titanic_logit)
#Get binary prediction with threshold 0.5
titanic_bin <- as.numeric(titanic_pred >= 0.5)

outcome_table <- table(titanic_bin, titanic_test$Survived)
#true/false negatives/positives
tp <- outcome_table[2,2]
fn <- outcome_table[1,2]
tn <- outcome_table[1,1]
fp <- outcome_table[2,1]
#overall accuarcy
(tp +tn) / (tp+tn +fp+fn)
#true positive rate
tp/(tp+fn)
#true negative rate
tn/(tn+fp)

####Example  2####
cancer <- read_csv("Data/pros.csv") %>%
  tibble()
#removing nas
cancer_2 <- cancer[complete.cases(cancer),] %>%
  select(-c("ID" ))
#converting necessary columns (1,3,4,5) to factors
cancer_2$CAPSULE <- as.factor(cancer_2$CAPSULE)
cancer_2$RACE <- as.factor(cancer_2$RACE)
cancer_2$DPROS <- as.factor(cancer_2$DPROS)
cancer_2$DCAPS <- as.factor(cancer_2$DCAPS)
cancer_2

###Model Construction
train_ind <- sample(1:nrow(cancer_2), 2/3*nrow(cancer_2))
cancer_train <- cancer_2[train_ind,]                    
cancer_test <- cancer_2[-train_ind,]
#creating the model
cancer_logit <- glm(CAPSULE~., data = cancer_train, family = binomial)
#running test data through model
cancer_pred <- predict(cancer_logit, cancer_test, type = "response")
#summary of fitted model
summary(cancer_logit)
thresh <- seq(from = 0.1, to = .99, by = 0.01)
FPR <- rep(0,length(thresh)) # 5-dim. vector of zeros
TPR <- rep(0, length(thresh))
for (i in 1:length(thresh)) {
  cancer_bin <- as.numeric(cancer_pred >= thresh[i])
  outcome <- table(cancer_bin, cancer_test$CAPSULE)
  print(outcome)
  FPR[i] <- outcome[2,1] / (outcome[2,1] + outcome[1,1])
  TPR[i] <- outcome[2,2] / (outcome[2,2] + outcome[1,2])
}
plot(FPR,TPR)
plot(thresh, FPR)
plot(thresh,TPR)
