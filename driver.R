library(tidyverse)
library(forestecology)
max_dist <- 7.5
cv_fold_size <- 100

bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size)

folds <- bigwoods %>%
  get_cv_fold_info(cv_fold_size)






# Running example
library(ggplot2)
bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size)

ggplot(data = bigwoods, aes(x = x, y = y)) +
  geom_point(aes(col = factor(fold))) +
  coord_fixed(ratio = 1) +
  geom_text(data = folds, aes(x = x, y = y, label = fold), size = 10)
