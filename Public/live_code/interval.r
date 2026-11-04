set.seed(42) # set seed

library(tidyverse)

df = read_csv("example.csv") # load data

# Data split

group = rsample::initial_split(df)

train = rsample::training(group)

test = rsample::testing(group)

model = lm(price ~ size + tenure + district + distance, train)

test$pred = predict(model, test)

test$error = test$price - test$pred

hist(test$error) # Draw histogram

hist(abs(test$error),breaks=100) # 

Q = quantile(abs(test$error), probs = 0.9)

test$lower = test$pred - Q
test$upper = test$pred + Q


# ctr+A -> ctr + Enter