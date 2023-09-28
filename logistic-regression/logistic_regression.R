setwd("./logistic-regression")
getwd()

install.packages("caret")
install.packages("ROCR")
install.packages("e1071")

library(caret)
library(ROCR)
library(e1071)

credit_dataset <- read.csv("credit_dataset_final.csv",
  header = TRUE, sep = ","
)
head(credit_dataset)
summary(credit_dataset)
str(credit_dataset)
View(credit_dataset)


to_factors <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}


scale_features <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- scale(df[[variable]], center = TRUE, scale = TRUE)
  }
  return(df)
}

numeric_vars <- c("credit.duration.months", "age", "credit.amount")
credit_dataset_scaled <- scale.features(credit_dataset, numeric_vars)


categorical_vars <- c(
  "credit.rating", "account.balance", "previous.credit.payment.status",
  "credit.purpose", "savings", "employment.duration", "installment.rate",
  "marital.status", "guarantor", "residence.duration", "current.assets",
  "other.credits", "apartment.type", "bank.credits", "occupation",
  "dependents", "telephone", "foreign.worker"
)

credit_dataset_final <- to_factors(
  df = credit_dataset_scaled, variables = categorical_vars
)
head(credit_dataset_final)
summary(credit_dataset_final)
View(credit_dataset_final)

# Create a train and test dataset
