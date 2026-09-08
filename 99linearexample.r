library(tidyverse)
set.seed(1)

library(tidyverse)

n <- 50
g <- 1
jump <- 10

set.seed(1)

data <- tibble(
  RoomNumber = sample(
    1:5,
    g * n,
    replace = TRUE,
    prob = c(rep(0.9 / 4, 4), 0.1)
  ),
  ID = str_c("データ", rep(seq(1, g), n))
) |>
  mutate(
    Price = 20 + if_else(RoomNumber == 5, jump, 0) +
      case_when(
        RoomNumber <= 4 ~ runif(g * n, -30, 30),
        RoomNumber == 5 ~ runif(g * n, -jump - 50, jump + 50)
      ),
    Price_True = 20 + if_else(RoomNumber == 5, jump, 0)
  )

data |>
  mutate(
    Mean = mean(Price),
    .by = c(RoomNumber, ID)
  ) |>
  mutate(
    Constant = mean(Price),
    .by = ID
  ) |>
  ggplot(
    aes(
      x = RoomNumber,
      y = Price
    )
  ) +
  geom_point(
    aes(
      y = Mean,
      color = "平均値"
    )
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  geom_point(
    aes(
      y = Price_True,
      color = "理想のモデル"
    )
  ) +
  theme_bw()
