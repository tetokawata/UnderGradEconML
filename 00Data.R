library(tidyverse)

data <- nanoparquet::read_parquet("Data/Tokyo_20053_20241.parquet") |>
  mutate(
    TradeYear = 取引時期 |>
      str_sub(1, 4) |>
      as.numeric()
  ) |>
  filter(
    str_detect(市区町村名, "区"),
    TradeYear >= 2020
  ) |>
  rename(
    Station = 最寄駅_名称,
    Area = 市区町村名,
    Size = 面積
  ) |>
  mutate(
    Price = 取引価格 / 1000000,
    Distance = 最寄駅_距離 |> as.numeric(),
    BuildYear = 建築年 |> str_sub(1, 4) |> as.numeric(),
    Tenure = TradeYear - BuildYear
  ) |>
  select(
    Station,
    Area,
    Size,
    Price,
    Distance,
    BuildYear,
    TradeYear,
    Tenure
  ) |>
  na.omit() |>
  mutate(
    Check = mean(D),
    .by = c(Area)
  )

estimatr::lm_robust(
  log(Price) ~ Line + (Area + Size + Distance + Tenure)^2 + I(Size^2) + I(Distance^2) + I(Tenure^2),
  data = data
) |>
  generics::tidy() |>
  filter(str_detect(term, "Line")) |>
  ggplot(
    aes(
      y = term,
      x = estimate,
      xmin = conf.low,
      xmax = conf.high
    )
  ) +
  geom_pointrange()

estimatr::lm_robust(
  log(Price) ~ D + (Area + Size + Distance + Tenure)^2 + I(Size^2) + I(Distance^2) + I(Tenure^2),
  data = data
) |>
  generics::tidy() |>
  filter(term == "D")

model <- ddml::ddml_plm(
  y = data$Price,
  D = data |> model.matrix(~ 0 + Line, data = _),
  X = data |> model.matrix(~ 0 + Area + Size + Distance + Tenure, data = _),
  learners = list(
    list(fun = ddml::ols)
  ),
  learners_DX = list(
    list(
      fun = ddml::mdl_glm,
      args = list(family = "binomial")
    )
  ),
  shortstack = TRUE,
  sample_folds = 2
)

model |> summary()

model <- ddml::ddml_ate(
  y = data$Price,
  D = data$D,
  X = data |> model.matrix(~ 0 + Area + Size + Distance + Tenure, data = _),
  learners = list(
    list(fun = ddml::ols)
  ),
  learners_DX = list(
    list(
      fun = ddml::mdl_glm
    ),
    list(fun = ddml::mdl_ranger)
  ),
  shortstack = TRUE
)

model$oos_pred$ED_X |> summary()

Z <- data |>
  model.matrix(~ Area + Size + Distance + Tenure, data = _) |>
  scale()

Z <- Z[, -1]

estimatr::lm_robust(model$psi_b ~ Z)

model <- grf::causal_forest(
  X = data |> model.matrix(~ 0 + Area + Size + Distance + Tenure, data = _),
  W = data$D,
  Y = data$Price |> log()
)

grf::average_treatment_effect(model, target.sample = "overlap")

grf::average_treatment_effect(model, target.sample = "control")

grf::average_treatment_effect(model, target.sample = "treat")

grf::average_treatment_effect(model)


model$predictions |> hist()

grf::best_linear_projection(
  model,
  data |>
    model.matrix(~ 0 + Area + Size + Distance + Tenure, data = _) |>
    scale()
)
