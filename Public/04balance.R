library(tidyverse)

data <- read_csv("data.csv")

estimatr::lm_robust(Price ~ Reform, data)

gtsummary::tbl_summary(data, by = Reform)

estimatr::lm_robust(Price ~ Reform + Tenure, data)

estimatr::lm_robust(
  Price ~ Reform + 
    Tenure + Size + Distance + District, 
  data)

estimatr::lm_robust(
  Price ~ Reform + 
    (Tenure + Size + Distance + District)^2 +
    I(Tenure^2) + I(Size^2) + I(Distance^2), 
  data)


# ctr + S -> Save
# ctr + A -> ctr + Enter 