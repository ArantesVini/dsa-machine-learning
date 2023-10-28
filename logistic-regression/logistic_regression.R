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
# TODO: continuar aqui
indexes <- sample(1:nrow(credit_dataset_final),
  size = 0.6 * nrow(credit_dataset_final)
)
train.data <- credit_dataset_final[indexes, ]
test.data <- credit_dataset_final[-indexes, ]
class(train.data)
class(test.data)

# Separando os atributos e as classes
test.feature.vars <- test.data[, -1]
test.class.var <- test.data[, 1]
class(test.feature.vars)

# Construindo o modelo de regressão logística
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
help(glm)
modelo_v1 <- glm(formula = formula.init, data = train.data, family = "binomial")

# Visualizando os detalhes do modelo
summary(modelo_v1)

# Fazendo previsões e analisando o resultado
previsoes <- predict(modelo_v1, test.data, type = "response")
previsoes <- round(previsoes)
View(previsoes)

# Confusion Matrix
confusionMatrix(table(data = previsoes, reference = test.class.var), positive = "1")

# Feature Selection
formula <- "credit.rating ~ ."
formula <- as.formula(formula)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)
importance <- varImp(model, scale = FALSE)

# Plot
plot(importance)

# Construindo um novo modelo com as variáveis selecionadas
formula.new <- "credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + savings + credit.duration.months"
formula.new <- as.formula(formula.new)
modelo_v2 <- glm(formula = formula.new, data = train.data, family = "binomial")

# Visualizando o novo modelo
summary(modelo_v2)

# Prevendo e Avaliando o modelo
previsoes_new <- predict(modelo_v2, test.data, type = "response")
previsoes_new <- round(previsoes_new)

# Confusion Matrix
confusionMatrix(table(data = previsoes_new, reference = test.class.var), positive = "1")


# Avaliando a performance do modelo

# Plot do modelo com melhor acurácia
modelo_final <- modelo_v2
previsoes <- predict(modelo_final, test.feature.vars, type = "response")
previsoes_finais <- prediction(previsoes, test.class.var)

# Função para Plot ROC
plot.roc.curve <- function(predictions, title.text) {
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,
    col = "black", lty = 1, lwd = 2,
    main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i"
  )
  abline(0, 1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc, 2)
  legend(0.4, 0.4, legend = c(paste0("AUC: ", auc)), cex = 0.6, bty = "n", box.col = "white")
}

# Plot
par(mfrow = c(1, 2))
plot.roc.curve(previsoes_finais, title.text = "Curva ROC")

# Fazendo previsões em novos dados

# Novos dados
account.balance <- c(1, 4, 3)
credit.purpose <- c(4, 2, 3)
previous.credit.payment.status <- c(3, 3, 2)
savings <- c(2, 3, 2)
credit.duration.months <- c(15, 12, 8)

# Cria um dataframe
novo_dataset <- data.frame(
  account.balance,
  credit.purpose,
  previous.credit.payment.status,
  savings,
  credit.duration.months
)

View(novo_dataset)

# Separa variáveis explanatórias numéricas e categóricas
new.numeric_vars <- c("credit.duration.months")
new.categorical_vars <- c(
  "account.balance", "previous.credit.payment.status",
  "credit.purpose", "savings"
)

# Aplica as transformações
novo_dataset_final <- to.factors(df = novo_dataset, variables = new.categorical_vars)
str(novo_dataset_final)

novo_dataset_final <- scale.features(novo_dataset_final, new.numeric_vars)
str(novo_dataset_final)

View(novo_dataset_final)

# Previsões
?predict
previsao_novo_cliente <- predict(modelo_final, newdata = novo_dataset_final, type = "response")
