# SetUp ----

library(tidyverse)
library(recipes)
library(SuperLearner)
library(hal9001)

data <- read_csv("Public/example.csv") |>
  mutate(
    E_Size = mean(Size),
    E_Tenure = mean(Tenure),
    E_Distance = mean(Distance),
    E_Kenpei = mean(Kenpei),
    E_Youseki = mean(Youseki),
    E_Reform = mean(Reform),
    .by = District
  ) |>
  select(
    Size,
    Tenure,
    Distance,
    Kenpei,
    Youseki,
    Reform,
    starts_with("E_"),
    District,
    Price,
    year_2024
  )

Target <- data |>
  filter(
    District %in% c("練馬区", "港区"),
    Size == 55,
    Tenure == 20,
    Kenpei == 60
  ) |>
  mutate(
    Youseki = 400,
    Distance = 15
  ) |>
  select(
    Size,
    Distance,
    Kenpei,
    Youseki,
    Reform,
    starts_with("E_")
  )


Test <- map_dfr(
  5:50,
  function(i) {
    result <- Target |>
      mutate(Tenure = i)
    return(result)
  }
)


Y <- data$Price

D <- data$year_2024

X <- data |>
  select(
    Size,
    Distance,
    Kenpei,
    Youseki,
    Reform,
    starts_with("E_"),
    Tenure
  )

# Prediction ----

Model_Y_D1 <- SuperLearner(
  Y = Y[D == 1],
  X = X[D == 1, ],
  newX = Test,
  SL.library = list(
    "SL.lm",
    "SL.ranger",
    "SL.gam",
    "SL.earth",
    "SL.hal9001"
  ),
  cvControl = SuperLearner.CV.control(V = 2),
  verbose = TRUE
)


Test |>
  mutate(
    District = if_else(
      E_Reform == max(E_Reform),
      "練馬",
      "港"
    )
  ) |>
  ggplot(
    aes(
      x = Tenure,
      y = Model_Y_D1$SL.predict,
      color = District
    )
  ) +
  geom_line(
    aes(
      linetype = "Stacking"
    )
  ) +
  geom_line(
    aes(
      y = Model_Y_D1$library.predict[, 1],
      linetype = "OLS"
    )
  ) +
  theme_bw()
