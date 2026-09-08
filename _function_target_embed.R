# SetUp ----

library(tidyverse)

nanoparquet::read_parquet("Data/Tokyo_20053_20241.parquet") |>
  glimpse()

test <- nanoparquet::read_parquet(
  "Data/Tokyo_20053_20241.parquet",
  col_select = c(
    "地区名",
    "最寄駅_距離",
    "取引時期",
    "最寄駅_名称",
    "市区町村名",
    "面積",
    "取引価格",
    "建築年",
    "都市計画"
  ),
)

test$取引時期 |> table()

data <- nanoparquet::read_parquet(
  "Data/Tokyo_20053_20241.parquet",
  col_select = c(
    "地区名",
    "最寄駅_距離",
    "取引時期",
    "最寄駅_名称",
    "市区町村名",
    "面積",
    "取引価格",
    "建築年",
    "都市計画"
  ),
) |>
  filter(str_detect(取引時期, "2024年第1四半期") |
    str_detect(取引時期, "2021年第1四半期")) |>
  filter(str_detect(市区町村名, "区")) |>
  rename(
    Station = 最寄駅_名称,
    Area = 市区町村名,
    Small = 地区名,
    Size = 面積,
    Distance = 最寄駅_距離,
    Zone = 都市計画
  ) |>
  mutate(
    D = if_else(str_detect(取引時期, "2024"), 1, 0),
    Price = (取引価格 / 1000000),
    Tenure = str_sub(建築年, 1, 4) |> as.numeric()
  ) |>
  select(Station, Area, Size, Price, D, Tenure, Distance, Small, Zone) |>
  na.omit()

data$D |> mean()

group <- rsample::initial_split(data, prop = 0.5)

train <- rsample::training(group)

test <- rsample::testing(group)

# Encoding ----

embed_value <- function(y, g, m) {
  pool_mean <- y |> mean()

  embed <- tibble(y, g) |>
    mutate(
      embed_naive = mean(y),
      n = n(),
      .by = g
    ) |>
    distinct(
      g,
      embed_naive,
      n
    ) |>
    mutate(
      embed = (n * embed_naive + m * pool_mean) / (n + m)
    ) |>
    select(
      embed,
      g
    )

  return(embed)
}

new_test <- test |>
  mutate(g = Small) |>
  left_join(
    embed_value(
      train$D,
      train$Small,
      10
    ),
    by = "g"
  ) |>
  rename(
    embed_small_D = embed
  ) |>
  mutate(
    g = Station
  ) |>
  left_join(
    embed_value(
      train$D,
      train$Station,
      10
    ),
    by = "g"
  ) |>
  rename(
    embed_station_D = embed
  ) |>
  mutate(
    g = Zone
  ) |>
  left_join(
    embed_value(
      train$D,
      train$Zone,
      10
    ),
    by = "g"
  ) |>
  rename(
    embed_zone_D = embed
  ) |>
  mutate(g = Small) |>
  left_join(
    embed_value(
      train$Price,
      train$Small,
      10
    ),
    by = "g"
  ) |>
  rename(
    embed_small_Y = embed
  ) |>
  mutate(
    g = Station
  ) |>
  left_join(
    embed_value(
      train$Price,
      train$Station,
      10
    ),
    by = "g"
  ) |>
  rename(
    embed_station_Y = embed
  ) |>
  mutate(
    g = Zone
  ) |>
  left_join(
    embed_value(
      train$Price,
      train$Zone,
      10
    ),
    by = "g"
  ) |>
  rename(
    embed_zone_Y = embed
  )


mean(new_test$D)

pred_D <- ranger::ranger(
  D ~ Size + Tenure + Distance + embed_small_D + embed_station_D + embed_zone_D + Area,
  new_test,
  importance = "impurity_corrected"
)

pred_Y <- ranger::ranger(
  Price ~ Size + Tenure + Distance + embed_small_Y + embed_station_Y + embed_zone_Y + Area,
  new_test,
  importance = "impurity_corrected"
)

pred_D |> ranger::importance()

pred_Y |> ranger::importance()

psu_Y <- new_test$Price - pred_Y$predictions

psu_D <- new_test$D - pred_D$predictions

estimatr::lm_robust(
  Price ~ D,
  new_test
) |>
  generics::tidy() |>
  filter(term == "D")

fixest::feols(
  Price ~ D + poly(Size, 2) + poly(Tenure, 2) + poly(Distance, 2) + Zone + Area + Zone + Small + Station,
  new_test
)

estimatr::lm_robust(
  Price ~ D + Size + Tenure + Distance + Zone + Area,
  new_test
) |>
  generics::tidy() |>
  filter(term == "D")

estimatr::lm_robust(psu_Y ~ 0 + psu_D)
