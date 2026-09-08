library(tidyverse)

set.seed(1)


Sim = function(n,i){
  set.seed(i)
  Temp = tibble(
    X = sample(
      seq(0,1,0.1),
      n,
      replace = TRUE
    ),
    TrueY = X^2 + 
      case_when(
        X < 0.4 ~ 0,
        .default = 2
      ),
    Y = TrueY + rnorm(n,0,5)
  )
  return(Temp)
}

Sim(100,1) |> 
  mutate(
    Mean = mean(Y),
    .by = X
  ) |> 
  mutate(
    OLS = lm(
      Y ~ X,
      data = pick(everything())
    ) |> 
      predict(
        pick(everything())
      ),
    OLS2 = lm(
      Y ~ poly(X,2),
      data = pick(everything())
    ) |> 
      predict(
        pick(everything())
      )
  ) |> 
  ggplot(
    aes(
      x = X,
      y = Y
    )
  ) +
  theme_bw() +
  geom_point(
    alpha = 0.2
  ) +
  geom_point(
    aes(
      y = TrueY,
      color = "母平均"
    )
  ) +
  geom_point(
    aes(
      y = Mean,
      color = "データ上の平均値"
    )
  ) +
  geom_point(
    aes(
      y = OLS,
      color = "OLS: Y ~ X"
    )
  ) +
  geom_point(
    aes(
      y = OLS2,
      color = "OLS: Y ~ X + X^2"
    )
  )
