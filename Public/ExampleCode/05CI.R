library(tidyverse)
library(estimatr)
library(dotwhisker)

Data = read_csv("Example.csv")

lm_robust(Price ~ 0 + District, Data)

Model = lm_robust(Price ~ 0 + District, Data)

dwplot(Model)


