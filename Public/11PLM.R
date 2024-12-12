set.seed(1)

library(tidyverse)
library(ddml)

Data = read_csv("Example.csv") # Use Data

Y = Data$Price # What Y?
D = Data$After # What D?
X = select(Data, Size,Tenure) # What X?

Model = ddml_plm(Y, D, data.matrix(X),
         list(list(fun = ols),
              list(fun = mdl_ranger)
              ),
         shortstack = TRUE
         )
summary(Model)
# ctr + A -> ctr + Enter