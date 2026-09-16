library(tidyverse)

df <- read_csv("example.csv")

lm(price ~ size + tenure, df)
