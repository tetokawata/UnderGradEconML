
library(hdm)
library(tidyverse)

Data = read_csv("Example.csv")

OLS = lm(Price ~ Size + District, Data) # OLS
LASSO = rlasso(Price ~ Size + District, Data, post = FALSE) # LASSO

PredLASSO = predict(LASSO, Data) # Prediction by LASSO
PredOLS = predict(OLS, Data) #  by OLS

mean((Data$Price - PredLASSO)^2) # Evaluation by LASSO
mean((Data$Price - PredOLS)^2) # by OLS


# Q1-1: OLSの方が簡単だから
