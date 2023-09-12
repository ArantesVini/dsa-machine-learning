# Loading initial dataset
dataset_bank <- read.table("categorical-feature-engineering/bank/bank-full.csv", header = TRUE, sep = ";")
View(dataset_bank)

library(dplyr)
library(ggplot2)

dataset_bank %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = job, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dataset_bank <- dataset_bank %>%
  mutate(
    technology_use =
      case_when(
        job == "admin" ~ "medio",
        job == "blue-collar" ~ "baixo",
        job == "entrepreneur" ~ "alto",
        job == "housemaid" ~ "baixo",
        job == "management" ~ "medio",
        job == "retired" ~ "baixo",
        job == "self-employed" ~ "baixo",
        job == "services" ~ "medio",
        job == "student" ~ "alto",
        job == "technician" ~ "alto",
        job == "unemployed" ~ "baixo",
        job == "unknown" ~ "baixo"
      )
  )

View(dataset_bank)

table(dataset_bank$technology_use)

round(prop.table(table(dataset_bank$technology_use)), 2)

dataset_bank <- dataset_bank %>%
  mutate(defaulted = ifelse(default == "yes", 1, 0))

View(dataset_bank)

library(caret)
?dummyVars
dmy <- dummyVars(" ~ .", data = dataset_bank)
bank.dummies <- data.frame(predict(dmy, newdata = dataset_bank))
View(bank.dummies)

View(dataset_bank)
str(bank.dummies)
View(bank.dummies)


dataset_bank %>%
  group_by(job, marital) %>%
  summarise(n = n())


dataset_bank %>%
  group_by(job, marital) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = job, y = n, fill = marital)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dmy <- dummyVars(~ job:marital, data = dataset_bank)
bank.cross <- predict(dmy, newdata = dataset_bank)
View(bank.cross)
