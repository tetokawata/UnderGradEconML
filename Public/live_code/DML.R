set.seed(111)

library(tidyverse)

df <- read_csv("example.csv")

estimatr::lm_robust(price ~ year, df)

estimatr::lm_robust(price ~ year + size + tenure, df) # same size and tenure

estimatr::lm_robust(
  price ~ year + size + tenure + distance + district + 
    I(size^2) + I(distance^2) + I(tenure^2), 
  df)

Y <- df$price 

X <- model.matrix(~ year + size + tenure + distance + district + 
               I(size^2) + I(distance^2) + I(tenure^2), 
             df)

X <- X[,-1]

hdm::rlassoEffects(
  x = X,
  y = Y,
  index = 1,
  method = "partialling out"
)


# ctr + A -> ctr + Enter
# ctr + S: save