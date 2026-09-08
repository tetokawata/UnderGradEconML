library(tidyverse)

set.seed(1)

data <- read_csv("Public/example.csv") |>
  filter(year_2024 == 1)

X <- data |>
  model.matrix(~Tenure,
    data = _
  )

object <- function(par) {
  weight <-
    par[1] * X[, 1] +
    par[2] * X[, 2]
  SSQ <- sum(weight[data$Reform == 1]^2) - 2 * sum(weight)
  return(SSQ)
}

est_parm <- optim(par = rep(1, ncol(X)), fn = object)$par

data$riesz_weight <-
  est_parm[1] * X[, 1] +
  est_parm[2] * X[, 2]

ps <- glm(
  Reform ~ Tenure,
  family = "binomial",
  data
) |>
  predict(data, "response")

summary(ps)

data$ipw_weight <- 1 / ps

data$pred_Y <- lm(
  Price ~ splines::bs(Tenure, 3),
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

result_weight <- result |>
  mutate(
    share_condition = sum(Reform),
    share_ipw = sum(ipw_weight),
    share_riesz = sum(riesz_weight),
    .by = c(Tenure, Reform)
  ) |>
  mutate(
    share = n(),
    .by = Tenure
  ) |>
  distinct(
    Tenure,
    Reform,
    ipw_weight,
    riesz_weight,
    share,
    share_condition,
    share_ipw,
    share_riesz
  ) |>
  filter(Reform == 1) |>
  arrange(Tenure) |>
  mutate(
    share = share / sum(share),
    share_condition = share_condition / sum(share_condition),
    share_ipw = share_ipw / sum(share_ipw),
    share_riesz = share_riesz / sum(share_riesz)
  )

result_weight |>
  ggplot(
    aes(
      x = Tenure,
      y = share
    )
  ) +
  geom_line(
    linetype = "dotted"
  ) +
  geom_line(
    aes(
      y = share_ipw,
      color = "IPW"
    )
  ) +
  geom_line(
    aes(
      y = share_riesz,
      color = "Riesz"
    )
  ) +
  theme_bw()

mean(result$Tenure)

mean(result$Tenure * result$ipw_weight)

mean(result$Tenure * result$riesz_weight)

estimatr::lm_robust(ipw_estimator ~ 1, result)

estimatr::lm_robust(riesz_estimator ~ 1, result)

estimatr::lm_robust(AIPW_estimator ~ 1, result)

estimatr::lm_robust(argment_riesz_estimator ~ 1, result)

result |>
  ggplot(
    aes(
      x = riesz_weight,
      y = ipw_weight
    )
  ) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = 1
  )
