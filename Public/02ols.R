library(tidyverse)

data = read_csv("data.csv")

lm(Price ~ Size, data)

lm(Price ~ Size + Tenure, data)

# ctr + A -> ctr + Enter