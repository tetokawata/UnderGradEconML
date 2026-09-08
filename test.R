library(tidyverse)

Data <- read_csv("Public/example.csv") |>
  filter(
    Tenure >= 10,
    year_2024 == 1
  ) |>
  mutate(
    Price = Price |> log(),
    District = if_else(
      District %in% c("中央区", "千代田区", "港区", "新宿区", "渋谷区", "文京区"),
      "中心3区",
      "その他"
    )
  )

Data$Tenure |> quantile(probs = 0.9)

Data$HatY <- Data |>
  lm(Price ~ splines::bs(Tenure, 6) + District + Tenure:District,
    data = _
  ) |>
  predict(Data)

Data$HatD <- Data |>
  lm(Reform ~ splines::bs(Tenure, 6) + District + Tenure:District,
    data = _
  ) |>
  predict(Data)

Data$B <- Data |>
  mutate(
    outcome = Price - HatY,
    treat = Reform - HatD
  ) |>
  lm(outcome ~ 0 + treat,
    data = _
  ) |>
  coef()

Data$B_Linear <- Data |>
  mutate(
    outcome = (Price - HatY) / (Reform - HatD),
    weight = (Reform - HatD)^2
  ) |>
  lm(outcome ~ Tenure + District,
    data = _,
    weight = weight
  ) |>
  predict(Data)

Data$B_Splines <- Data |>
  mutate(
    outcome = (Price - HatY) / (Reform - HatD),
    weight = (Reform - HatD)^2
  ) |>
  lm(outcome ~ splines::bs(Tenure, 3) + District + District:Tenure,
    data = _,
    weight = weight
  ) |>
  predict(Data)

Data |> summary()

Data$RY_D1 <- Data$HatY + Data$B * (1 - Data$HatD)
Data$RY_D0 <- Data$HatY + Data$B * (0 - Data$HatD)

Data$RY_D1_Linear <- Data$HatY + Data$B_Linear * (1 - Data$HatD)
Data$RY_D0_Linear <- Data$HatY + Data$B_Linear * (0 - Data$HatD)

Data$RY_D1_Splines <- Data$HatY + Data$B_Splines * (1 - Data$HatD)
Data$RY_D0_Splines <- Data$HatY + Data$B_Splines * (0 - Data$HatD)

Data$HatY_D1 <- Data |>
  lm(Price ~ splines::bs(Tenure, 6) + District + Tenure:District,
    data = _,
    subset = Reform == 1
  ) |>
  predict(Data)

Data$HatY_D0 <- Data |>
  lm(Price ~ splines::bs(Tenure, 6) + District + Tenure:District,
    data = _,
    subset = Reform == 0
  ) |>
  predict(Data)

Data |>
  ggplot(
    aes(
      x = Tenure,
      y = HatY_D1
    )
  ) +
  geom_line(
    aes(
      color = "D1",
      linetype = "T"
    )
  ) +
  geom_line(
    aes(
      y = HatY_D0,
      color = "D0",
      linetype = "T"
    )
  ) +
  geom_line(
    aes(
      y = RY_D1_Linear,
      color = "D1",
      linetype = "R: Linear"
    )
  ) +
  geom_line(
    aes(
      y = RY_D0_Linear,
      color = "D0",
      linetype = "R: Linear"
    )
  ) +
  theme_bw() +
  facet_wrap(~District)

Data |>
  ggplot(
    aes(
      x = Tenure,
      y = HatY_D1 - HatY_D0
    )
  ) +
  geom_bin2d(
    aes(
      color = "T"
    )
  ) +
  geom_line(
    aes(
      y = RY_D1_Linear - RY_D0_Linear,
      color = "R: Linear"
    )
  ) +
  geom_line(
    aes(
      y = RY_D1_Splines - RY_D0_Splines,
      color = "R: Splines"
    )
  ) +
  theme_bw() +
  facet_wrap(~District)
