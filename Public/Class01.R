library(tidyverse)
library(rpart)
library(rpart.plot)

Data <- read_csv("Data.csv")

Fit <- rpart(Price ~ Tenure + Size, Data)

rpart.plot(Fit)

Fit2 <- rpart(Tenure ~ .,
      Data,
      control = rpart.control(
        maxdepth = 10,
        cp = 0,
        minbucket = 1,
        minsplit = 1
      ))

rpart.plot(Fit2)
