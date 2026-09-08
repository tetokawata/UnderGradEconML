
# SetUP ----

set.seed(11)
library(tidyverse)


EstBoost <- function(train_y, train_x, test_x) {
  group = sample(
    1:2,
    length(train_y),
    replace = TRUE,
    prob = c(0.8,0.2)
  )
  
  train = lightgbm::lgb.Dataset(
    train_x[group == 1,] |> data.matrix(),
    label = train_y[group == 1]
  )
  
  validate = lightgbm::lgb.Dataset(
    train_x[group == 2,] |> data.matrix(),
    label = train_y[group == 2]
  )
  
  pred = lightgbm::lgb.train(
    data = train,
    params = list(
      objective = "regression",
      learning_rate = 0.001
    ),
    nrounds = 50000,
    valids = list(test = validate),
    early_stopping_rounds = 100,
    verbose = 0
  ) |> 
    predict(test_x |> 
              data.matrix())
  
  return(pred)
}


Data = read_csv("Public/Example.csv") |> 
  mutate(
    EmebedSize = mean(Size),
    EmbedTenure = mean(Tenure),
    EmbedDistance = mean(Size),
    .by = District
  )

Group = sample(
  1:2,
  nrow(Data),
  replace = TRUE
)

# Nuisance ----

Data$HatY_D1_OLS = lm(
  Price ~ (Tenure + Size + Distance + District)**2 + 
    splines::bs(Tenure,3) + splines::bs(Size,3) + splines::bs(Distance,3), 
  Data,
  subset = Group == 1 & D == 1
) |> 
  predict(Data) |> 
  as.numeric()

Data$HatY_D0_OLS = lm(
  Price ~ (Tenure + Size + Distance + District)**2 + 
    splines::bs(Tenure,3) + splines::bs(Size,3) + splines::bs(Distance,3), 
  Data,
  subset = Group == 1 & D == 0
) |> 
  predict(Data) |> 
  as.numeric()

Data$HatD_OLS = lm(
  D ~ (Tenure + Size + Distance + District)**2 + 
    splines::bs(Tenure,3) + splines::bs(Size,3) + splines::bs(Distance,3), 
  Data,
  subset = Group == 1
) |> 
  predict(Data) |> 
  as.numeric()

Data$HatY_D1_RF = ranger::ranger(
  Price ~ Tenure + Size + Distance + EmebedSize + EmbedTenure + EmbedDistance,
  Data[Group == 1 & Data$D == 1,]
) |> 
  predict(Data) |> 
  magrittr::extract2("predictions") |> 
  as.numeric()

Data$HatY_D0_RF = ranger::ranger(
  Price ~ Tenure + Size + Distance + EmebedSize + EmbedTenure + EmbedDistance,
  Data[Group == 1 & Data$D == 0,]
) |> 
  predict(Data) |> 
  magrittr::extract2("predictions") |> 
  as.numeric()

Data$HatD_RF = ranger::ranger(
  D ~ Tenure + Size + Distance + EmebedSize + EmbedTenure + EmbedDistance,
  Data[Group == 1,]
) |>
  predict(Data) |> 
  magrittr::extract2("predictions") |> 
  as.numeric()

Data$HatY_D1_GBM = EstBoost(
  train_y = Data$Price[Group == 1 & Data$D == 1],
  train_x = Data[Group == 1 & Data$D == 1,] |> 
    select(
      Tenure,
      Size,
      Distance,
      EmebedSize,
      EmbedTenure,
      EmbedDistance
    ),
  test_x = Data |> 
    select(
      Tenure,
      Size,
      Distance,
      EmebedSize,
      EmbedTenure,
      EmbedDistance
    )
)

Data$HatY_D0_GBM = EstBoost(
  train_y = Data$Price[Group == 1 & Data$D == 0],
  train_x = Data[Group == 1 & Data$D == 0,] |> 
    select(
      Tenure,
      Size,
      Distance,
      EmebedSize,
      EmbedTenure,
      EmbedDistance
    ),
  test_x = Data |> 
    select(
      Tenure,
      Size,
      Distance,
      EmebedSize,
      EmbedTenure,
      EmbedDistance
    )
)

Data$HatD_GBM = EstBoost(
  train_y = Data$D[Group == 1],
  train_x = Data[Group == 1,] |> 
    select(
      Tenure,
      Size,
      Distance,
      EmebedSize,
      EmbedTenure,
      EmbedDistance
    ),
  test_x = Data |> 
    select(
      Tenure,
      Size,
      Distance,
      EmebedSize,
      EmbedTenure,
      EmbedDistance
    )
)


Data$HatY_D1 = lm(Price ~ HatY_D1_RF + HatY_D1_OLS + HatY_D1_GBM,
                  Data,
                  subset = Group == 2 & D == 1) |> 
  predict(Data)|> 
  as.numeric()

Data$HatY_D0 = lm(Price ~ HatY_D0_RF + HatY_D0_OLS + HatY_D0_GBM,
                  Data,
                  subset = Group == 2 & D == 0) |> 
  predict(Data)|> 
  as.numeric()

Data$HatD = lm(D ~ HatD_OLS + HatD_RF + HatD_GBM,
               Data,
               subset = Group == 2) |> 
  predict(Data)|> 
  as.numeric()


Weight = lmw::lmw(
  ~ D + (Tenure + Size + Distance + District)**2 + 
    splines::bs(Tenure,3) + splines::bs(Size,3) + splines::bs(Distance,3), 
  Data,
  method = "MRI"
)

mean((Weight$weights*Data$Price)[Data$D == 1]) - mean((Weight$weights*Data$Price)[Data$D == 0])

Plugin = Data$HatY_D1 - Data$HatY_D0

Adjust = mean((Weight$weights*(Data$Price - Data$HatY_D1))[Data$D == 1 & Group == 2]) - 
  mean((Weight$weights*(Data$Price - Data$HatY_D0))[Data$D == 0 & Group == 2])

Score = Plugin + Adjust

estimatr::lm_robust(Score ~ 1)
