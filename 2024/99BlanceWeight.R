
# SetUp ----

set.seed(111)
library(tidyverse)

#read_csv("Data/Tokyo_20053_20241.csv") |> 
#  arrow::write_parquet(
#    "Data/Tokyo_20053_20241.parquet")

## Data ----

arrow::open_dataset("Data/Tokyo_20053_20241.parquet")

Data = arrow::read_parquet(
  "Data/Tokyo_20053_20241.parquet"
  ) |> 
  mutate(
    Price = 取引価格/(10^6),
    BuildY = 建築年 |> 
      str_sub(1,4) |> 
      as.numeric(),
    TradeY = 取引時期 |> 
      str_sub(1,4) |> 
      as.numeric(),
    TradeQ = 取引時期 |> 
      str_sub(7,7) |> 
      as.numeric(),
    Tenure = TradeY - BuildY,
    Size = 面積,
    Distance = 最寄駅_距離 |> 
      as.numeric(),
    District = 市区町村名,
    D = (TradeY == 2023) |> 
      if_else(1,0),
    CBD = case_when(
      District %in% c("港区","新宿区","渋谷区","中央区","千代田区","文京区") ~ 1,
      .default = 0
    )
  ) |> 
  filter(
    TradeY %in% c(2021,2023),
    str_detect(District,"区"),
    Tenure <= 50,
    Tenure >= 3,
    Size >= 40,
    Size <= 100
  ) |> 
  select(
    Price:CBD
  ) |> 
  na.omit()

## Split ----

Group = sample(
  1:2,
  Data |> nrow(),
  replace = TRUE
)

# Comparison ----
# 
## OLS----

FitOLS = lmw::lmw(
  ~ CBD + 
    (Size + Distance + Tenure)**2 +
    I(Size^2) + I(Distance^2) + I(Tenure^2),  
  Data, 
  method = "MRI")

Fig1 = Data |> 
  mutate(
    Weight = FitOLS$weights
  ) |> 
  ggplot(
    aes(
      x = Price,
      fill = CBD |> factor()
    )
  ) +
  geom_histogram(
    aes(
      y = ..density..
    ),
    position = "identity",
    alpha = 0.5
  ) +
  ylim(0,0.05)

Fig2 = Data |> 
  mutate(
    Weight = FitOLS$weights
  ) |> 
  ggplot(
    aes(
      x = Price,
      fill = CBD |> factor()
    )
  ) +
  geom_histogram(
    aes(
      y = ..density..,
      weight = Weight
    ),
    position = "identity",
    alpha = 0.5
  ) +
  ylim(0,0.05)

Fig3 = Data |> 
  mutate(
    Weight = FitOLS$weights
  ) |> 
  ggplot(
    aes(
      x = Size,
      fill = CBD |> factor()
    )
  ) +
  geom_histogram(
    aes(
      y = ..density..
    ),
    position = "identity",
    alpha = 0.5
  ) +
  ylim(0,0.1)

Fig4 = Data |> 
  mutate(
    Weight = FitOLS$weights
  ) |> 
  ggplot(
    aes(
      x = Size,
      fill = CBD |> factor()
    )
  ) +
  geom_histogram(
    aes(
      y = ..density..,
      weight = Weight
    ),
    position = "identity",
    alpha = 0.5
  ) +
  ylim(0,0.1)

cowplot::plot_grid(Fig1,Fig2,Fig3,Fig4, ncol = 2)

## Simulation ----
## 

N = 2000

Data = tibble(
  D = sample(0:1, N, replace = TRUE),
  X = 2*D + rnorm(N,0,5),
  Y = X + rnorm(N)
)


Data |> 
  ggplot(
    aes(
      x = Y,
      fill = D |> 
        factor()
    )
  ) +
  geom_histogram(
    position = "identity",
    alpha = 0.5
  )

Data |> 
  mutate(
    Weight = WeightIt::weightit(
      D ~ poly(X,2),
      data = pick(D,X),
      method = "ebal"
    ) |> 
      magrittr::extract2("weights")
  )  |> 
  ggplot(
    aes(
      x = X,
      fill = D |> 
        factor(),
      weights = Weight
    )
  ) +
  geom_histogram(
    alpha = 0.5,
    position = "identity"
  )


Data |> 
  mutate(
    Weight = WeightIt::weightit(
      D ~ X,
      data = pick(D,X),
      method = "ebal"
    ) |> 
      magrittr::extract2("weights")
  ) |> 
  estimatr::lm_robust(
    D ~ X,
    data = _
    )

Data |> 
  mutate(
    Weight = WeightIt::weightit(
      D ~ X,
      data = pick(D,X),
      method = "ebal"
    ) |> 
      magrittr::extract2("weights")
  ) |> 
  ggplot(
    aes(
      x = Y,
      fill = D |> 
        factor()
    )
  ) +
  geom_histogram(
    aes(
      weights = Weight
    ),
    position = "identity",
    alpha = 0.5
  )

## Simulation ----
## 

Data = arrow::read_parquet(
  "Data/Tokyo_20053_20241.parquet"
) |> 
  mutate(
    Price = 取引価格/(10^6),
    BuildY = 建築年 |> 
      str_sub(1,4) |> 
      as.numeric(),
    TradeY = 取引時期 |> 
      str_sub(1,4) |> 
      as.numeric(),
    TradeQ = 取引時期 |> 
      str_sub(7,7) |> 
      as.numeric(),
    Tenure = TradeY - BuildY,
    Size = 面積,
    Distance = 最寄駅_距離 |> 
      as.numeric(),
    District = 市区町村名,
    D = (TradeY == 2023) |> 
      if_else(1,0),
    CBD = case_when(
      District %in% c("港区","新宿区","渋谷区","中央区","千代田区","文京区") ~ 1,
      .default = 0
    )
  ) |> 
  filter(
    TradeY %in% c(2021,2023),
    str_detect(District,"区"),
    Tenure <= 50,
    Tenure >= 3,
    Size >= 40,
    Size <= 100
  ) |> 
  select(
    Price:CBD
  ) |> 
  na.omit()



Data |> 
  ggplot(
    aes(
      x = Size,
      fill = CBD |> 
        factor()
    )
  ) +
  geom_histogram(
    position = "identity",
    alpha = 0.5
  )

Data |> 
  mutate(
    Weight = WeightIt::weightit(
      CBD ~ Size,
      data = pick(CBD,Size),
      method = "ebal"
    ) |> 
      magrittr::extract2("weights")
  ) |> 
  mutate(
    ResD = lm(
      CBD ~ factor(Size),
      data = pick(CBD,Size),
      weights = Weight
    ) |> 
      magrittr::extract2("residuals")
  ) |> 
  ggplot(
    aes(
      x = ResD,
      fill = CBD |> 
        factor()
    )
  ) +
  geom_histogram()


Data |> 
  mutate(
    Weight = WeightIt::weightit(
      D ~ X,
      data = pick(D,X),
      method = "ebal"
    ) |> 
      magrittr::extract2("weights")
  ) |> 
  estimatr::lm_robust(
    D ~ X,
    data = _
  )

Data |> 
  mutate(
    Weight = WeightIt::weightit(
      D ~ X,
      data = pick(D,X),
      method = "ebal"
    ) |> 
      magrittr::extract2("weights")
  ) |> 
  ggplot(
    aes(
      x = Y,
      fill = D |> 
        factor()
    )
  ) +
  geom_histogram(
    aes(
      weights = Weight
    ),
    position = "identity",
    alpha = 0.5
  )
