#install.packages("tidymodels")
#install.packages("tidyverse")

library(tidymodels)
library(tidyverse)
library(skimr)
library(moderndive)


mtcars %>% skim()
m <- lm(data = mtcars, mpg ~ disp + hp + drat + wt)
m
get_regression_table(m)
get_regression_points(m)


iris %>% skim()

iris %>% rep_sample_n(size = 4, reps=4)
