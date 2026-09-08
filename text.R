library(tidyverse)
library(quanteda)
library(quanteda.textplots)

data <- read_csv("text.csv")

corp <- corpus(data, text_field = "Title")

token <- tokens(corp)

dfm <- dfm(token)

dfm_trim <- dfm_select(dfm, min_nchar = 2)

textplot_wordcloud(dfm_trim)

X <- convert(dfm_trim, "matrix")

Y <- data$Y

LASSO <- hdm::rlasso(x = X, y = Y, post = FALSE)

table(LASSO$index)

coef(LASSO)[LASSO$index]
