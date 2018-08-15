suppressPackageStartupMessages(library(tidyverse))
library(forestecology)
max_dist <- 7.5
cv_fold_size <- 100
model_number <- 4

bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size)

# CV fold/grid info
folds <- bigwoods %>%
  get_cv_fold_info(cv_fold_size)

# Modeling and species stuff
model_specs <- get_model_specs(bigwoods, model_number)

# Focal vs comp main dataframe for analysis
focal_vs_comp <- bigwoods %>%
  create_focal_vs_comp(max_dist, folds, model_specs)

# Fit and predict
posterior_param <- focal_vs_comp %>%
  fit_bayesian_model(model_specs)
focal_vs_comp <- focal_vs_comp %>%
  predict_bayesian_model(model_specs, posterior_param)







# Running example
library(ggplot2)
bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size)

ggplot(data = bigwoods, aes(x = x, y = y)) +
  geom_point(aes(col = factor(fold))) +
  coord_fixed(ratio = 1) +
  geom_text(data = folds, aes(x = x, y = y, label = fold), size = 10)
