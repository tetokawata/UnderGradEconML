set.seed(1)

library(tidyverse)
library(SuperLearner)
library(estimatr)
library(dotwhisker)

Data <- read_csv("Data.csv")

Fit <- lm_robust(Price ~ ., Data, alpha = 0.005)

dwplot(Fit, ci = 0.995)

# Partialling-out

Y <- Data$Price
D <- Data$After
X <- select(Data,-Price,-After,-Disrict)

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

ResY <- Y - FitY$SL.predict
ResD <- D - FitD$SL.predict

Fit <- lm_robust(ResY ~ ResD)

dwplot(Fit, ci = 0.995)

