# SetUp ----

library(tidyverse)

nanoparquet::read_parquet("Data/Tokyo_20053_20241.parquet") |>
  glimpse()

data <- nanoparquet::read_parquet(
  "Data/Tokyo_20053_20241.parquet",
  col_select = c("最寄駅_距離", "取引時期", "最寄駅_名称", "市区町村名", "面積", "取引価格", "建築年"),
) |>
  filter(str_detect(取引時期, "2023") |
    str_detect(取引時期, "2021")) |>
  filter(str_detect(市区町村名, "区")) |>
  rename(
    Station = 最寄駅_名称,
    Area = 市区町村名,
    Size = 面積,
    Distance = 最寄駅_距離
  ) |>
  mutate(
    D = if_else(str_detect(取引時期, "2021"), 1, 0),
    Price = 取引価格 / 1000000,
    Tenure = str_sub(建築年, 1, 4) |> as.numeric()
  ) |>
  select(Station, Area, Size, Price, D, Tenure, Distance) |>
  na.omit()

data |>
  gtsummary::tbl_summary(
    by = D
  )

group <- rsample::initial_split(data, prop = 0.5)

train <- rsample::training(group)
test <- rsample::testing(group)

# Encoding ----

y <- train$Price

g <- train$Station

m <- 10

pool_mean <- y |> mean()

Embed <- tibble(y, g) |>
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
    embed_naive,
    embed,
    Station,
    n,
    Area
  )

Embed |>
  ggplot(
    aes(
      y = reorder(Station, embed),
      x = embed
    )
  ) +
  geom_point() +
  geom_point(
    aes(
      x = embed_naive
    ),
    alpha = 0.2
  ) +
  theme_bw()


# Multi-stage Encoding ----

m <- 100

group_mean <- train |>
  mutate(
    mean = mean(Price),
    .by = Area
  ) |>
  distinct(
    Area,
    mean
  )

group_Embed <- train |>
  mutate(
    embed_naive = mean(Price),
    n = n(),
    .by = Station
  ) |>
  distinct(
    Station,
    Area,
    embed_naive,
    n
  ) |>
  left_join(
    group_mean,
    by = "Area"
  ) |>
  mutate(
    group_embed = (n * embed_naive + m * mean) / (n + m)
  ) |>
  select(
    group_embed,
    Station,
    n,
    Area
  )


Embed |>
  inner_join(
    group_Embed,
    by = c("Station", "n", "Area")
  ) |>
  ggplot(
    aes(
      y = reorder(Station, group_embed),
      x = group_embed
    )
  ) +
  geom_point() +
  geom_point(
    aes(
      x = embed,
      color = Area
    )
  )

# Comparison ----

new_test <- test |>
  left_join(
    group_mean,
    by = "Area"
  ) |>
  left_join(
    Embed,
    by = c("Station", "Area")
  ) |>
  left_join(
    group_Embed,
    by = c("Station", "Area")
  ) |>
  mutate(
    embed = if_else(is.na(embed), mean, embed),
    group_embed = if_else(is.na(group_embed), mean, group_embed)
  )

new_train <- train |>
  left_join(
    group_mean,
    by = "Area"
  ) |>
  left_join(
    Embed,
    by = c("Station", "Area")
  ) |>
  left_join(
    group_Embed,
    by = c("Station", "Area")
  ) |>
  mutate(
    embed = if_else(is.na(embed), mean, embed),
    group_embed = if_else(is.na(group_embed), mean, group_embed)
  )

ranger::ranger(
  Price ~ Area + Size + Tenure + Distance,
  new_train
)

ranger::ranger(
  Price ~ embed + Size + Tenure + Distance,
  new_train
)


ranger::ranger(
  Price ~ embed + Size + Area + Tenure + Distance,
  new_train
)


ranger::ranger(
  Price ~ group_embed + Size + Tenure + Distance,
  new_train
)
