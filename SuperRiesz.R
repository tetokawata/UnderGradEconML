# pak::pak("herbps10/SuperRiesz")

library(SuperRiesz)
library(tidyverse)

N <- 1e3

set.seed(152)

data <- tibble(
  W = runif(N, -1, 1),
  A = rbinom(N, 1, plogis(W)),
  Y = rnorm(N, mean = W + A, sd = 0.5)
)

vars <- c("W", "A")

m <- function(alpha, data) {
  alpha(data("treatment")) - alpha(data("control"))
}

fit <- super_riesz(
  data[, vars],
  list(
    "control" = mutate(data[, vars], A = 0),
    "treatment" = mutate(data[, vars], A = 1)
  ),
  library = c("linear", "nn"),
  m = m
)

predict(fit, data[, vars])
