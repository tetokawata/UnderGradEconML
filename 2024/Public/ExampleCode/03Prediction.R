set.seed(1)

library(tidyverse)

Data = read_csv("Example.csv")

N = nrow(Data)

T = round(N*0.8)

Group = sample(
  1:N,
  T
)
Train = Data[Group,]
Test = Data[-Group,]

OLS = lm(Price ~ Tenure + Size, Train)
OLS_Long = lm(Price ~ poly(Tenure,2) + poly(Size,2), Train)

Test$Pred = predict(OLS,Test)
Test$Pred_Long = predict(OLS_Long,Test)

mean((Test$Price - Test$Pred)^2)
mean((Test$Price - Test$Pred_Long)^2)

# ctr + A -> ctr (cmd) + Enter 
# ctr + S
# Un do (巻き戻し) -> ctr + Z