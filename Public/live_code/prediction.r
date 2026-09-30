library(tidyverse)

df = read_csv("example.csv")

# Data split

group = rsample::initial_split(df)

train = rsample::training(group)

test = rsample::testing(group)

# LASSO

lasso = hdm::rlasso(price ~ ., train, post = FALSE)

# Test

test$lasso = predict(lasso, test)

mean((test$price - test$lasso)^2)

# ctr + A -> ctr + Enter (run)
# ctr + Z (undo) 
# ctr + S (save)