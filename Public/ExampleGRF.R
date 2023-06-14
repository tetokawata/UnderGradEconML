set.seed(1)

library(tidyverse)
library(SuperLearner)
library(estimatr)
library(dotwhisker)
library(grf)

Data <- read_csv("Data.csv")

# Partialling-out

Y <- Data$Price
D <- Data$Reform
X <- select(Data,Size,Tenure)

FitY <- CV.SuperLearner(
  Y = Y,
  X = X,
  SL.library = c("SL.lm","SL.ranger"),
  V = 2
)

FitD <- CV.SuperLearner(
  Y = D,
  X = X,
  SL.library = c("SL.lm","SL.ranger"),
  V = 2
)

FitTau <- causal_forest(
  X = X,
  Y = Y,
  W = D, # 見たい説明変数を指定
  Y.hat = FitY$SL.predict, # Yの予測値
  W.hat = FitD$SL.predict # Dの予測値
)

hist(FitTau$predictions)

Result <- mutate(
  Data,
  Pred = FitTau$predictions
)

predict(
  FitTau,
  tibble(Size = 50, Tenure = 30)
  )
