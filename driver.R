suppressPackageStartupMessages(library(tidyverse))
library(forestecology)
max_dist <- 7.5
cv_fold_size <- 100
model_number <- 4

bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size) %>%
  # Convert to factor
  mutate(
    species = factor(species),
    family_phylo = factor(family_phylo),
    trait_group = factor(trait_group)
  )

# CV fold/grid info
folds <- bigwoods %>%
  get_cv_fold_info(cv_fold_size)

# Modeling and species stuff
model_specs <- get_model_specs(bigwoods, model_number)

# Focal vs comp main dataframe for analysis
focal_vs_comp <- bigwoods %>%
  create_focal_vs_comp(max_dist, folds, model_specs)

# Fit and predict for all trees
posterior_param <- focal_vs_comp %>%
  fit_bayesian_model(model_specs)
focal_vs_comp <- focal_vs_comp %>%
  predict_bayesian_model(model_specs, posterior_param)

# Fit and predict using crossvalidation:
n_folds <- max(focal_vs_comp$fold)
focal_vs_comp$growth_hat <- NA
for(i in 1:n_folds){
  train <- focal_vs_comp %>%
    filter(fold != i)
  test <- focal_vs_comp %>%
    filter(fold == i)

  # Fit and predict for current fold
  posterior_param <- train %>%
    fit_bayesian_model(model_specs)
  focal_vs_comp$growth_hat[focal_vs_comp$fold == i] <- test %>%
    predict_bayesian_model(model_specs, posterior_param) %>%
    pull(growth_hat)

  print(i)
}

ggplot(focal_vs_comp, aes(growth, growth_hat, alpha = 0.2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", size = 1) +
  facet_wrap(~species, nrow = 2) +
  coord_fixed()

