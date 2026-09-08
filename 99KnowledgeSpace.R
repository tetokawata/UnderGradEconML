library(tidyverse)

data <- read_csv("Data/test_202510106.csv") |>
  mutate(
    A1 = if_else(Q1 == -50, 1, 0),
    A2 = if_else(Q2 == 100, 1, 0)
  )

data |> summary()

if_else(data$A1 == 1 & data$A2 == 1, 1, 0) |> mean()

if_else(data$A1 == 1 & data$A2 == 0, 1, 0) |> mean()

if_else(data$A1 == 0 & data$A2 == 1, 1, 0) |> mean()

if_else(data$A1 == 0 & data$A2 == 0, 1, 0) |> mean()
