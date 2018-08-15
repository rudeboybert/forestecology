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
notion_of_focal_species <- model_specs$notion_of_focal_species
notion_of_competitor_species <- model_specs$notion_of_competitor_species
model_formula <- model_specs$model_formula
species_of_interest <- model_specs$species_of_interest

# Focal vs comp main dataframe for analysis
focal_vs_comp <- create_focal_vs_comp(
  bigwoods, max_dist, folds, notion_of_focal_species, notion_of_comp_species
)

posterior_param <- fit_bayesian_model(focal_vs_comp)
focal_vs_comp <- focal_vs_comp %>%
  predict_bayesian_model(posterior_param)







# Running example
library(ggplot2)
bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size)

ggplot(data = bigwoods, aes(x = x, y = y)) +
  geom_point(aes(col = factor(fold))) +
  coord_fixed(ratio = 1) +
  geom_text(data = folds, aes(x = x, y = y, label = fold), size = 10)
