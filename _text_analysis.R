library(quanteda)
library(tidyverse)
library(text2vec)

data <- read.delim("Data/LiteratureList.tsv") |>
  select(
    出版年月日,
    種別,
    論文タイトル
  ) |>
  rename(
    Year = 出版年月日,
    Type = 種別,
    Title = 論文タイトル
  ) |>
  mutate(
    D = if_else(Type == "一般", 1, 0),
    Y = Year |> as.numeric()
  ) |>
  na.omit()

data |> write_csv("Public/text.csv")

data |>
  quanteda::corpus(
    text_field = "Title"
  ) |>
  quanteda::tokens() |>
  quanteda::dfm() |>
  quanteda::dfm_trim(min_docfreq = 5) |>
  quanteda::dfm_select(min_nchar = 2) |>
  quanteda.textplots::textplot_wordcloud()

X <- data |>
  quanteda::corpus(
    text_field = "Title"
  ) |>
  quanteda::tokens() |>
  quanteda::dfm() |>
  quanteda::dfm_trim(min_docfreq = 5) |>
  quanteda::dfm_select(min_nchar = 2) |>
  quanteda::convert("data.frame")

X <- X[, -1]

X |> summary()

data |>
  ggplot(
    aes(
      x = Y,
      fill = D |>
        factor()
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    alpha = 0.5,
    position = "identity"
  )

model_Y <- hdm::rlasso(
  y = data$Y,
  x = X
)

model_D <- hdm::rlasso(
  y = data$D,
  x = X
)

model_Y$index[model_Y$index == TRUE]

model_Y |> summary()

model_D$index[model_D$index == TRUE]

model_D |> summary()


estimatr::lm_robust(Y ~ D, data)

model_Tau <- hdm::rlassoEffect(
  x = X |> data.matrix(),
  y = data$Y,
  d = data$D,
  method = "double selection"
)

model_Tau |> summary()

model_Tau |> confint()

model_Tau$selection.index[model_Tau$selection.index == TRUE]

X[, model_Tau$selection.index] |> summary()

estimatr::lm_robust(
  Y ~ D + .,
  X[, model_Tau$selection.index] |>
    mutate(
      Y = data$Y,
      D = data$D
    ),
  se_type = "stata"
)

model_PLM <- ddml::ddml_plm(
  y = data$Y,
  D = data$D,
  X = X |> data.matrix(),
  shortstack = TRUE,
  learners = list(
    list(fun = ddml::ols),
    list(fun = ddml::mdl_glmnet),
    list(fun = ddml::mdl_ranger)
  ),
  learners_DX = list(
    list(
      fun = ddml::mdl_glm,
      args = list(family = "binomial")
    ),
    list(
      fun = ddml::mdl_glmnet,
      args = list(family = "binomial")
    ),
    list(fun = ddml::mdl_ranger)
  )
)

model_AIPW <- ddml::ddml_ate(
  y = data$Y,
  D = data$D,
  X = X |> data.matrix(),
  shortstack = TRUE,
  learners = list(
    list(fun = ddml::ols),
    list(fun = ddml::mdl_glmnet),
    list(fun = ddml::mdl_ranger)
  ),
  learners_DX = list(
    list(
      fun = ddml::mdl_glm,
      args = list(family = "binomial")
    ),
    list(
      fun = ddml::mdl_glmnet,
      args = list(family = "binomial")
    ),
    list(fun = ddml::mdl_ranger)
  )
)

model_PLM |> summary()
model_AIPW |> summary()
