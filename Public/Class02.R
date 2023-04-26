library(tidyverse)
library(rpart)
library(rpart.plot)

Data <- read_csv("Data.csv")

Fit <- rpart(Price ~ ., Data[1:2000,])

rpart.plot(Fit)

Pred <- predict(Fit, Data)

mean((Data$Price - Pred)[2001:4173]^2)

mean((Data$Price - 40)[2001:4173]^2)
