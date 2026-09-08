library(tidyverse)
library(estimatr)
library(dotwhisker)
library(cobalt)

Data = read_csv("Example.csv")

Sum = bal.tab(After ~ Price + Tenure + Size + District,
        Data)

love.plot(Sum)

Model0 = lm_robust(
  Price ~ After,
  Data # 今年と去年の単純比較
)

lm_robust(
  Price ~ After + factor(Size),
  Data
)

lm_robust(
  Price ~ After + Size,
  Data
) # Sizeの平均値がバランス

lm_robust(
  Price ~ After + Size + Tenure + DistanceStation + District,
  Data
) # Size,Tenure,Distance,Districtの平均値がバランス

Model1 = lm_robust(
  Price ~ After + factor(Size) + Tenure + DistanceStation + District,
  Data
)# Tenure,Distance,Districtの平均値がバランス, Sizeは完璧なバランス
　
Model2 = lm_robust(
  Price ~ After + (Size + Tenure + DistanceStation + District)**2 +
    I(Size^2) + I(Tenure^2) + I(DistanceStation^2),
  Data) # Size,Tenure,Distance,Districtの平均値と分散と共分散がバランス

dwplot(
  list(Model1,Model2),
  vars_order = c("After")
  )

# ctr + A -> ctr + Enter