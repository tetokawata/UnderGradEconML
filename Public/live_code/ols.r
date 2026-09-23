library(tidyverse)

df = read_csv("example.csv")

model = lm(price ~ size + tenure, df)

model_long = lm(price ~ ., df)

df$pred = predict(model, df)

df$pred_long = predict(model_long, df)

# ctr + A -> ctr + Enter (run)
# ctr + Z (undo) 
# ctr + S (save)