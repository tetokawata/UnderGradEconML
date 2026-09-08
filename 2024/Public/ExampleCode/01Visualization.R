library(tidyverse)

data("iris") # Import Data

ggplot(iris, 
       aes(x = Petal.Length,
           y = Petal.Width)
       ) + geom_point()

ggplot(iris, 
       aes(x = Petal.Length,
           y = Petal.Width)
) + geom_bin2d()
# ctr + A -> ctr (cmd) + Enter 
# ctr + S