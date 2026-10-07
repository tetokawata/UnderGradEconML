set.seed(42) # set seed

library(tidyverse)

df = read_csv("example.csv") # load data

# Data split

group = rsample::initial_split(df)

train = rsample::training(group)

test = rsample::testing(group)

# LASSO

lasso = hdm::rlasso(price ~ ., train, post = FALSE)

ols = lm(price ~ ., train)

# Test


test$lasso = predict(lasso, test)

test$ols = predict(ols,test)

mean((test$price - test$ols)^2)

mean((test$price - test$lasso)^2)

# ctr + A -> ctr + Enter (run)
# ctr + Z (undo) 
# ctr + S (save)