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
    Tenure,
    Distance,
    Kenpei,
    Youseki,
    Reform,
    starts_with("E_")
  )


Test <- map_dfr(
  15:105,
  function(i) {
    result <- Target |>
      mutate(Size = i)
    return(result)
  }
)


Y <- data$Price

D <- data$year_2024

X <- data |>
  select(
    Tenure,
    Distance,
    Kenpei,
    Youseki,
    Reform,
    starts_with("E_"),
    Size
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
  cvControl = SuperLearner.CV.control(V = 10),
  verbose = TRUE
)



# Difference ----

Model_Y <- CV.SuperLearner(
  Y = Y,
  X = X,
  SL.library = list(
    "SL.lm",
    "SL.ranger",
    "SL.gam",
    "SL.earth"
  ),
  verbose = TRUE,
  cvControl = list(V = 2),
  innerCvControl = list(list(V = 2))
)

Model_D <- CV.SuperLearner(
  Y = D,
  X = X,
  SL.library = list(
    "SL.lm",
    "SL.ranger",
    "SL.gam",
    "SL.earth"
  ),
  verbose = TRUE,
  cvControl = list(V = 2),
  innerCvControl = list(list(V = 2))
)

Hat_Y <- Model_Y$SL.predict
Hat_D <- Model_D$SL.predict

psu_Y <- (Y - Hat_Y) / (D - Hat_D)

psu_Weight <- (D - Hat_D)^2

Model_B <- SuperLearner(
  X = X,
  Y = psu_Y,
  newX = Test,
  obsWeights = psu_Weight,
  SL.library = list(
    "SL.lm",
    "SL.mean",
    "SL.ranger",
    "SL.gam",
    "SL.earth"
  )
)


Test$Hat_B <- Model_B$SL.predict |>
  as.numeric()

Test$Hat_Y_OLS <- Model_Y_D1$library.predict[, 1] |>
  as.numeric()

Test$Hat_Y_D1 <-
  Model_Y_D1$SL.predict |>
  as.numeric()

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
      x = Size,
      y = Hat_B,
      color = District
    )
  ) +
  geom_line() +
  theme_bw()

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
      x = Size,
      y = Hat_Y_D1,
      color = District
    )
  ) +
  geom_line() +
  theme_bw()

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
      x = Size,
      y = Hat_Y_OLS,
      color = District
    )
  ) +
  geom_line(
    aes(
      linetype = "OLS"
    )
  ) +
  geom_line(
    aes(
      y = Hat_Y_D1,
      linetype = "ML"
    )
  ) +
  theme_bw()

Test |>
  write_csv("Figure/ExamplePrediction.csv")
