set.seed(111)

library(tidyverse)

df <- read_csv("example.csv")

split <- rsample::initial_split(df)

train <- rsample::training(split)

test <- rsample::testing(split)

ols <- lm(price ~ size + tenure, train)
test$pred_ols <- predict(ols, test)

rf <- ranger::ranger(price ~ size + tenure, train)
test$pred_rf <- predict(rf, test)$predictions

predict(rf, test, predict.all = TRUE)$predictions
