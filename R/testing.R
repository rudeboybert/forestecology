# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/

library(tidymodels)
library(randomForest)
library(ranger)


# Training/testing split:
iris <- iris %>%
  as_tibble()
iris_split <- iris %>%
  initial_split(prop = 0.6)
iris_split %>%
  training()
iris_split %>%
  testing()


# Preprocess data
iris_recipe <- iris_split %>%
  training() %>%
  recipe(Species ~ .) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

iris_testing <- iris_recipe %>%
  bake(iris_split %>% testing())
iris_training <- iris_recipe %>%
  juice()


# Apply model using two different implementations of same method: random forests
iris_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Species ~ ., data = iris_training)

iris_rf <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Species ~ ., data = iris_training)


# Make predictions
iris_ranger %>%
  predict(iris_testing)

iris_rf %>%
  predict(iris_testing)

iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing)


# Validation: get multiclass metrics
iris_ranger %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)

iris_rf %>%
  predict(iris_testing) %>%
  bind_cols(iris_testing) %>%
  metrics(truth = Species, estimate = .pred_class)

# Validation: get binary metrics
iris_probs <- iris_ranger %>%
  predict(iris_testing, type = "prob") %>%
  bind_cols(iris_testing)

iris_probs %>%
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()

iris_ranger %>%
  predict(iris_testing, type = "prob") %>%
  bind_cols(predict(iris_ranger, iris_testing)) %>%
  bind_cols(select(iris_testing, Species))


predict(iris_ranger, iris_testing, type = "prob") %>%
  bind_cols(predict(iris_ranger, iris_testing)) %>%
  bind_cols(select(iris_testing, Species)) %>%
  metrics(Species, .pred_setosa:.pred_virginica, estimate = .pred_class)
