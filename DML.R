library(tidyverse)

n <- 2000

data <- tibble(
  X = runif(n, -5, 5)
) |>
  mutate(
    D = if_else(
      X >= 0,
      sample(0:1, n, replace = TRUE, prob = c(0.1, 0.9)),
      sample(0:1, n, replace = TRUE, prob = c(0.9, 0.1))
    ),
    Y = X + D + rnorm(n)
  )

pred_Y <- lm(Y ~ X, data, subset = D == 1) |>
  predict(data)

pred_D <- glm(D ~ X, family = binomial, data) |>
  predict(
    data,
    "response"
  )

tau_plugin <- mean(pred_Y)

tau_ipw <- mean(data$D * data$Y / pred_D)

tau_adjust <- mean(data$D * pred_Y / pred_D)

tau_aipw <- tau_plugin + tau_ipw - tau_adjust

# Data

data <- read_csv("Public/example.csv")

Y <- data$Price

D <- data$Reform

X <- data |>
  select(
    -Price,
    -Reform
  )

pred_Y <- lm(Y ~ (.)**2, X, subset = D == 1) |>
  predict(X)

pred_D <- glm(D ~ ., family = binomial, X) |>
  predict(X, "response")

summary(pred_D)

tau_plugin <- mean(pred_Y)

tau_ipw <- mean(D * Y / pred_D)

adjust <- mean(D * pred_Y / pred_D)

tau_plugin + tau_ipw - adjust
