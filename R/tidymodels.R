#install.packages("tidymodels")
#install.packages("tidyverse")

library(tidymodels)
library(tidyverse)
library(broom)
library(conflicted)

# conflict_prefer("filter", winner = "dplyr")
tidymodels_prefer(quiet = FALSE)

# https://www.tmwr.org/base-r.html 
# ------------------------------------------------------------------------------
# Chapter 3 tidymodels 

corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

corr_res[[1]] %>% 
  tidy()


corr_res %>% 
  map_dfr(tidy,  .id = "predictor")

corr_res %>% 
  # Convert each to a tidy format; `map_dfr()` stacks the data frames 
  map_dfr(tidy, .id = "predictor") %>% 
  ggplot(aes(x = fct_reorder(predictor, estimate))) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with mpg")



split_by_species <- 
  crickets %>% 
  group_nest(species) 

split_by_species$data


model_by_species <- 
  split_by_species %>% 
  mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))
model_by_species


model_by_species %>% 
  mutate(coef = map(model, tidy)) %>% 
  select(species, coef) %>% 
  unnest(cols = c(coef))



# ---------------------------------------------------------------------------------
# Ch 4

data(ames)

ames

p1 <- ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white")
p1
# log transform: no negative valued predictions, stabilize variance, 
p1 +scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


# ---------------------------------------------------------------------------------
# Ch 5
# Splitting data


set.seed(501)
# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

# stratified sample
# With a strata argument, the random sampling is conducted within the stratification variable. 
# This can help ensure that the resamples have equivalent proportions as the original data set. 
# For a categorical variable, sampling is conducted separately within each class. For a numeric stratification variable,
# strata is binned into quartiles, which are then used to stratify. Strata below 10% of the total are pooled together; see make_strata() for more details.
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

# ?make_strata()
# ---------------------------------------------------------------------------------
# Ch 6 Fitting Models with parsnip


