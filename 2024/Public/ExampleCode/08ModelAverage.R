set.seed(1)

library(tidyverse)
library(ranger)

Data = read_csv("Example.csv")

N = nrow(Data)
T = round(N*0.8)
Group = sample(
  1:N,
  T
)
Train = Data[Group,]
Test = Data[-Group,]

Model = ranger(Price ~ Size + Tenure, Train)
Pred = predict(Model,Test)
Pred$predictions
