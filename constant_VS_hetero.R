# SetUp ----

library(tidyverse)

data <- read_csv("Public/small.csv") |>
  filter(
    District %in% c("港", "練馬")
  ) |>
  mutate(
    D = if_else(District == "港", 1, 0)
  )

# Simple ----

result <- data |>
  mutate(
    Mean_Y = mean(Price),
    .by = c("D", "Size")
  ) |>
  distinct(
    Mean_Y,
    D,
    Size
  ) |>
  pivot_wider(
    names_from = D,
    values_from = Mean_Y
  ) |>
  mutate(
    naive = `1` - `0`
  )

result$pred_cf <- grf::causal_forest(
  Y = data$Price,
  W = data$D,
  X = data |>
    select(Size)
) |>
  predict(result |> select(Size)) |>
  magrittr::extract2("predictions")


result |>
  ggplot(
    aes(
      x = Size,
      y = naive
    )
  ) +
  geom_point(
    aes(
      color = "Naive"
    )
  ) +
  geom_point(
    aes(
      y = pred_cf,
      color = "Causal Forest"
    )
  )

# Complex ----


result <- data |>
  mutate(
    Mean_Y = mean(Price),
    .by = c("D", "RoomNumber", "Reform")
  ) |>
  distinct(
    Mean_Y,
    D,
    RoomNumber,
    Reform
  ) |>
  pivot_wider(
    names_from = D,
    values_from = Mean_Y
  ) |>
  mutate(
    naive = `1` - `0`
  )


result$pred_cf <- grf::causal_forest(
  Y = data$Price,
  W = data$D,
  X = data |>
    select(
      RoomNumber,
      Reform
    )
) |>
  predict(result |> select(
    RoomNumber,
    Reform
  )) |>
  magrittr::extract2("predictions")


result |>
  ggplot(
    aes(
      x = pred_cf,
      y = naive
    )
  ) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = 1
  )
