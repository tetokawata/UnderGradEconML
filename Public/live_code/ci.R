library(tidyverse)

df = read_csv("example.csv") # load data

# model = estimatr::lm_robust(tenure ~ 0 + district, df)

model = estimatr::lm_robust(
  size ~ 0 + district, 
  df, 
  alpha = 0.004) # Risk of lose

dotwhisker::dwplot(model, ci = 0.95) # Win rate
# ctr + a -> ctr + Enter