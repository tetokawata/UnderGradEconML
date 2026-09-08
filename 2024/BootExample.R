
SimData <- function(i, n, s = 5) {
  set.seed(i)
  
  Size = SupportSize |> 
    c(sample(
      SupportSize,
      n - length(SupportSize),
      replace = TRUE
    ))
  
  TruePrice = case_when(
    Size <= 50 ~ 0,
    Size >= 51 & Size <= 70 ~ 5,
    Size >= 71 ~ 7
  )
  
  Temp = tibble(
    TruePrice,
    Size
  ) |> 
    mutate(
      Price = TruePrice + runif(n,-s,s),
      Type = "Constant"
    )
  return(Temp)
}

set.seed(10)

Temp = SimData(1,100,s=5) |> 
  bind_rows(
    tibble(
    Size = 100,
    TruePrice = 7,
    Price = 50)
  )

mosaic::resample(Temp)

mosaic::resample(Temp) |> 
  mutate(
    Data = "1",
    Pred = rpart::rpart(
      Price ~ Size,
      data = pick(everything()),
      control = rpart::rpart.control(
        maxdepth = 30,
        cp = 0,
        minbucket = 1,
        minsplit = 1
      )
    ) |> 
      predict(pick(everything()))
  ) |> 
  bind_rows(
    mosaic::resample(Temp) |> 
      mutate(
        Data = "2",
        Pred = rpart::rpart(
          Price ~ Size,
          data = pick(everything()),
          control = rpart::rpart.control(
            maxdepth = 30,
            cp = 0,
            minbucket = 1,
            minsplit = 1
          )
        ) |> 
          predict(pick(everything()))
      )
  ) |> 
  bind_rows(
    mosaic::resample(Temp) |> 
      mutate(
        Data = "3",
        Pred = rpart::rpart(
          Price ~ Size,
          data = pick(everything()),
          control = rpart::rpart.control(
            maxdepth = 30,
            cp = 0,
            minbucket = 1,
            minsplit = 1
          )
        ) |> 
          predict(pick(everything()))
      )
  ) |> 
  bind_rows(
    mosaic::resample(Temp) |> 
      mutate(
        Data = "4",
        Pred = rpart::rpart(
          Price ~ Size,
          data = pick(everything()),
          control = rpart::rpart.control(
            maxdepth = 30,
            cp = 0,
            minbucket = 1,
            minsplit = 1
          )
        ) |> 
          predict(pick(everything()))
      )
  ) |> 
  bind_rows(
    mosaic::resample(Temp) |> 
      mutate(
        Data = "5",
        Pred = rpart::rpart(
          Price ~ Size,
          data = pick(everything()),
          control = rpart::rpart.control(
            maxdepth = 30,
            cp = 0,
            minbucket = 1,
            minsplit = 1
          )
        ) |> 
          predict(pick(everything()))
      )
  ) |> 
  bind_rows(
    mosaic::resample(Temp) |> 
      mutate(
        Data = "6",
        Pred = rpart::rpart(
          Price ~ Size,
          data = pick(everything()),
          control = rpart::rpart.control(
            maxdepth = 30,
            cp = 0,
            minbucket = 1,
            minsplit = 1
          )
        ) |> 
          predict(pick(everything()))
      )
  )|> 
  ggplot(
    aes(
      x = Size,
      y = Price
    )
  ) +
  geom_point() +
  geom_line(
    aes(
      y = Pred
    )
  ) +
  facet_wrap(~Data,ncol = 2)

