library(tidyverse)

Data = read_csv("Example.csv")

lm(Price ~ Tenure + Size, Data)

# ctr + A -> ctr (cmd) + Enter 
# ctr + S