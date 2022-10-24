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



linear_reg() %>% set_engine("lm")%>% translate()
linear_reg() %>% set_engine("stan")%>% translate()
linear_reg(penalty = 1) %>% set_engine("glmnet")%>% translate()


lm_model <- 
  linear_reg() %>% 
  set_engine("lm")
lm_model

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_xy_fit


# Random forest

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

lm_form_fit %>% extract_fit_engine() %>% vcov()

lm_form_fit %>% extract_fit_engine() %>% plot

model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

model_res
param_est <- coef(model_res)
class(param_est)
param_est

# better way to summarise model fit:
tidy(model_res)

# Predictions 

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)


ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int")) 



tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))

# parsnip_addin()


# ---------------------------------------------------------------------------------
# Ch 7 A Model Workflow 

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% update_formula(Sale_Price ~ Longitude)

# adding raw variables to workflow instead of formula:
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

#install.packages("multilevelmod")
# library(multilevelmod)
# 
# multilevel_spec <- linear_reg() %>% set_engine("lmer")
# 
# multilevel_workflow <- 
#   workflow() %>% 
#   # Pass the data along as-is: 
#   add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
#   add_model(multilevel_spec, 
#             # This formula is given to the model
#             formula = distance ~ Sex + (age | Subject))
# 
# multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
# multilevel_fit


# install.packages("censored")
library(censored)

parametric_spec <- survival_reg()

parametric_workflow <- 
  workflow() %>% 
  add_variables(outcome = c(fustat, futime), predictors = c(age, rx)) %>% 
  add_model(parametric_spec, 
            formula = Surv(futime, fustat) ~ age + strata(rx))

parametric_fit <- fit(parametric_workflow, data = ovarian)
parametric_fit


# define many formulas beforehand:
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)


workflowsets::workflow_set()

location_models <- workflow_set(preproc = location, models = list(lm = lm_model))
location_models
location_models$info[[1]]
extract_workflow(location_models, id = "coords_lm")

location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models$fit[[1]]

#  EVALUATING THE TEST SET
# will fit the model to the entire training set and evaluate it with the testing set
final_lm_res <- last_fit(lm_wflow, ames_split)
final_lm_res

fitted_lm_wflow <- extract_workflow(final_lm_res)
collect_metrics(final_lm_res)
collect_predictions(final_lm_res) %>% slice(1:5)

# ------------------------------------------------------------------------------
# Ch 8 Feature Engineering with recipes


simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames
# advantage of recipes:
# These computations can be recycled across models since they are not tightly coupled to the modeling function.
# A recipe enables a broader set of data processing choices than formulas can offer.
# The syntax can be very compact. For example, all_nominal_predictors() can be used to capture many variables for specific types of processing while a formula would require each to be explicitly listed.
# All data processing can be captured in a single R object instead of in scripts that are repeated, or even spread across different files.


lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  remove_formula() %>% 
  add_recipe(simple_ames)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit
# The predict() method applies the same preprocessing that was used on the training set to the new data before passing them along to the model’s predict() method
predict(lm_fit, ames_test %>% slice(1:3))

lm_fit %>% 
  # This returns the parsnip object:
  extract_fit_parsnip() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)



simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  # the bottom 1% of the neighborhoods will be lumped into a new level called “other.”
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())



# INTERACTION TERMS

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # : creates interaction termm between Gr_Liv_Area and everyting starting with Bldg_Type_
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )





ames_rec <- recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  #  SPLINE FUNCTIONS
  step_ns(Latitude, deg_free = 20) %>% 
  # PCA
  step_pca(matches("(SF$)|(Gr_Liv)"))


tidy(ames_rec)


ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)
estimated_recipe <- 
  lm_fit %>% 
  extract_recipe(estimated = TRUE)

tidy(estimated_recipe, id = "my_id")


# COLUMN ROLES

# ames_rec %>% update_role(address, new_role = "street address")
# When a formula is used with the initial call to recipe() it assigns roles to each of the columns, 
# depending on which side of the tilde they are on. 
# Those roles are either "predictor" or "outcome". However, other roles can be assigned as needed.
# 
# For example, in our Ames data set, the original raw data contained a column for address.
# It may be useful to keep that column in the data so that, after predictions are made,
# problematic results can be investigated in detail. In other words,
# the column could be important even when it isn’t a predictor or outcome.

# -------------------------------------------------------------------------------
# Ch 9 Judging Model Effectiveness




