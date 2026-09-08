library(tidyverse)

set.seed(1)

data <- read_csv("Public/example.csv")

X <- data |>
  model.matrix(~ splines::bs(Tenure, 6),
    data = _
  )

object <- function(par) {
  weight <-
    par[1] * X[, 1] +
    par[2] * X[, 2] +
    par[3] * X[, 3] +
    par[4] * X[, 4] +
    par[5] * X[, 5] +
    par[6] * X[, 6] +
    par[7] * X[, 7]
  SSQ <- sum(weight[data$Reform == 1]^2) - 2 * sum(weight)
  return(SSQ)
}

est_parm <- optim(par = rep(1, 7), fn = object)$par

data$riesz_weight <- est_parm[1] + X[, 1] +
  est_parm[2] * X[, 2] +
  est_parm[3] * X[, 3] +
  est_parm[4] * X[, 4] +
  est_parm[5] * X[, 5] +
  est_parm[6] * X[, 6] +
  est_parm[7] * X[, 7]

ps <- glm(Reform ~ splines::bs(Tenure), family = "binomial", data) |>
  predict(data, "response")

data$ipw_weight <- 1 / ps

data$pred_Y <- lm(
  Price ~ splines::bs(Tenure, 4),
  data,
  subset = Reform == 1
) |>
  predict(data)

result <- data |>
  mutate(
    riesz_weight = if_else(
      Reform == 1,
      riesz_weight,
      0
    ),
    ipw_weight = if_else(
      Reform == 1,
      ipw_weight,
      0
    )
  ) |>
  mutate(
    ipw_estimator = ipw_weight * Price,
    riesz_estimator = riesz_weight * Price
  ) |>
  mutate(
    AIPW_estimator = pred_Y + ipw_weight * (Price - pred_Y),
    argment_riesz_estimator = pred_Y + riesz_weight * (Price - pred_Y)
  )

result |>
  ggplot(
    aes(
      x = Tenure
    )
  ) +
  geom_density(
    aes(
      fill = "Original"
    )
  ) +
  geom_density(
    aes(
      x = Tenure,
      weight = riesz_estimator,
      fill = "Riesz"
    ),
    alpha = 0.5
  ) +
  geom_density(
    aes(
      x = Tenure,
      weight = ipw_estimator,
      fill = "IPW"
    ),
    alpha = 0.5
  )

estimatr::lm_robust(ipw_estimator ~ 1, result)

estimatr::lm_robust(riesz_estimator ~ 1, result)

estimatr::lm_robust(AIPW_estimator ~ 1, result)

estimatr::lm_robust(argment_riesz_estimator ~ 1, result)

result |>
  filter(
    Reform == 1
  ) |>
  ggplot(
    aes(
      x = riesz_weight,
      y = ipw_weight
    )
  ) +
  geom_bin2d() +
  geom_abline(
    intercept = 0,
    slope = 1
  )
