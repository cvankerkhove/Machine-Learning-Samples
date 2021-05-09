#File Name: Basic Regression.R
#Description: This file contains a sample for construction a linear model and
#improving accuracy by eliminating high leverage points and outliers
#Reference to the Textbook ISLR (James)
#Author(s): Chris VanKerkhove

#LINEAR REGRESSION
setwd("~/Desktop/ML Samples")
####Data Preprocess####
# On Toyota Data
library(readr)
corollas <- read_csv("ToyotaCorolla.csv")
library(tibble)
corollas <- tibble(corollas)
#missing values
mis <- 0
for(col.name in colnames(corollas)){
  print(col.name)
  mis <- sum(is.na(corollas[[col.name]])) + mis
}
print(mis)

#new df
corollas2 <- corollas[, c("Price", "Age_08_04", "KM", "Fuel_Type", "HP", "Met_Color", "Doors", "Quarterly_Tax", "Weight")]
is.factor(corollas2$Fuel_Type)
corollas2$Fuel_Type <- as.factor(corollas2$Fuel_Type)
levels(corollas2$Fuel_Type)
is.factor(corollas2$Met_Color)
corollas2$Met_Color <- as.factor(corollas2$Met_Color)
levels(corollas2$Met_Color)

#####Model Construction####
corollas_lm = lm(formula=Price~., data=corollas2)
summary(corollas_lm)

#ggplot2 library
library(ggplot2)
ggplot(corollas2, aes(x=Age_08_04, y = Price)) +
  geom_point() +
  stat_smooth(method="lm", color="red")

#leverage points
lev <- hatvalues(corollas_lm)
plot(lev, type="h")
#student residuals
studRes <- rstudent(corollas_lm)

####Improving R^2 Value####
##Removing Predictor with largest p-value##
library(dplyr)
library(rlist)
library(stringr)
p_vals <- summary(corollas_lm)$coefficients[,4]
name <- names(which.max(p_vals))[1]
name2 <- substr(name,0, str_length(name)-1)
corollas3 <- select(corollas2, -c(name2))
corollas_lm2 = lm(formula=Price~., data=corollas3)
summary(corollas_lm2)
summary(corollas_lm)

##Removing Outlier and high leverage points
res <- studRes[studRes < 3]

cutoff <- 10 * ((length(p_vals) + 1) / length(corollas2$Price))
lev2 <- lev[lev < cutoff]

new_data <- mutate(corollas2, Student.Residuals = studRes, Leverage = lev) %>%
  filter(Student.Residuals %in% res & Leverage %in% lev2)
corollas_lm3 = lm(formula=Price~., data=new_data)
summary(corollas_lm3)