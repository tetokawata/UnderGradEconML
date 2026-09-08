library(tidyverse)

n <- 20
g <- 4

set.seed(1)

data <- tibble(
  RoomNumber = sample(1:5, g * n, replace = TRUE),
  ID = rep(seq(1, g), n)
) |>
  mutate(
    Price = 20 + 10 * RoomNumber +
      case_when(
        RoomNumber <= 4 ~ runif(g * n, -30, 30),
        RoomNumber == 5 ~ runif(g * n, -60, 60)
      )
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
  geom_smooth(
    aes(y = Constant),
    method = "lm",
    se = FALSE
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ poly(x, 2),
    se = FALSE
  ) +
  facet_wrap(~ID) +
  theme_bw()
