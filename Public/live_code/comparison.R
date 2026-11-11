library(tidyverse)

df = read_csv("example.csv")

estimatr::lm_robust(
  price ~ reform,
  df
)

tab = cobalt::bal.tab(
  reform ~ price + size + tenure + distance + district,
  df
)

cobalt::love.plot(tab)

estimatr::lm_robust(
  price ~ reform + size + tenure + distance + district,
  df
)


estimatr::lm_robust(
  price ~ reform + size + tenure + distance + district +
    I(size^2) + I(tenure^2) + I(distance^2),
  df
)

estimatr::lm_robust(
  price ~ reform + (size + tenure + distance + district)^2 +
    I(size^2) + I(tenure^2) + I(distance^2),
  df
)

# Selection

hdm::rlasso(
  price ~ reform + size + tenure + distance + district +
    I(size^2) + I(tenure^2) + I(distance^2),
  df
)



# ctr + A -> ctr + Enter
# ctr + S