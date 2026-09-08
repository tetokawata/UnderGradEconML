# SetUp----

library(tidyverse)
library(recipes)


lambda <- 500000
target <- 4

data <- read_csv("Public/example.csv") |>
  filter(
    year_2024 == 1
  )

# RF ----

model_RF <- grf::regression_forest(
  Y = data$Price,
  X = data |> select(RoomNumber)
)

weight_RF <- model_RF |>
  grf::get_forest_weights(
    tibble(RoomNumber = target)
  )

data$weight_RF <- weight_RF[1, ]

# OLS ----

X <- model.matrix(~RoomNumber, data)

Test <- c(1, target) |> matrix(nrow = 1)

data$weight_OLS <- (t(solve(t(X) %*% X) %*% t(X)) %*% t(Test)) |>
  as.numeric()

# Ridge ----

X <- model.matrix(~ RoomNumber + I(RoomNumber^2) + I(RoomNumber^3) + I(RoomNumber^4), data)

Test <- c(1, target, target^2, target^3, target^4) |> matrix(nrow = 1)

Penalty <- c(0, rep(lambda, ncol(X) - 1)) * diag(ncol(X))

data$weight_Ridge <- (t(solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X)) %*% t(Test)) |>
  as.numeric()


sum(data$weight_Ridge)
sum(data$weight_OLS)

# Result -----

data |>
  ggplot(
    aes(
      x = Price |> log(),
      y = weight_RF
    )
  ) +
  geom_point() +
  facet_wrap(~RoomNumber)


data |>
  mutate(
    weight_Mean = 1 / nrow(data)
  ) |>
  mutate(
    weight_RF = sum(weight_RF),
    weight_Mean = sum(weight_Mean),
    weight_OLS = sum(weight_OLS),
    weight_Ridge = sum(weight_Ridge),
    .by = RoomNumber
  ) |>
  distinct(
    RoomNumber,
    weight_RF,
    weight_Mean,
    weight_OLS,
    weight_Ridge
  ) |>
  ggplot(
    aes(
      x = RoomNumber,
      y = weight_RF
    )
  ) +
  geom_line(
    aes(
      color = "RF"
    )
  ) +
  geom_line(
    aes(
      y = weight_Mean,
      color = "Mean"
    )
  ) +
  geom_line(
    aes(
      y = weight_OLS,
      color = "OLS"
    )
  ) +
  geom_line(
    aes(
      y = weight_Ridge,
      color = "Ridge"
    )
  ) +
  geom_hline(
    yintercept = 0
  ) +
  theme_bw()
