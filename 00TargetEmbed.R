library(tidyverse)
library(recipes)

train <- tibble(
  Y = runif(3),
  G = c("A", "B", "B")
)

test <- tibble(
  Y = runif(1),
  G = c("A", "C")
)

Embed <- train |>
  recipe(
    Y ~ G
  ) |>
  embed::step_lencode(
    c(G),
    outcome = vars(Y),
    smooth = TRUE
  ) |>
  prep()

Embed |>
  bake(new_data = test)

Embed |>
  bake(new_data = NULL)

train$Y |> mean()

train$Y[train$G == "B"] |> mean()
