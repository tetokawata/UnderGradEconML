set.seed(1)

library(tidyverse)

data = read_csv("data.csv")

train = sample(1:5474, 4000) # ctr + Enter

data_train = data[train,]
data_test = data[-train,]

model1 = lm(Price ~ Size, data_train)

model2 = lm(Price ~ Size + Tenure + Distance + District, data_train)

model3 = hdm::rlasso(Price ~ ., data_train, post = FALSE) # LASSO

pred1 = predict(model1, data_test)

pred2 = predict(model2, data_test)

pred3 = predict(model3, data_test) # LASSO

eval1 = mean((data_test$Price - pred1)^2)

eval2 = mean((data_test$Price - pred2)^2)

eval3 = mean((data_test$Price - pred3)^2) # LASSO

(eval1 - eval2)/eval1

# ctr + A -> ctr + Enter