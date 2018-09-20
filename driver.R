suppressPackageStartupMessages(library(tidyverse))
library(forestecology)
max_dist <- 7.5
cv_fold_size <- 100
model_number <- 4

# Set up bigwoods data
bigwoods <- bigwoods %>%
  define_bigwoods_buffer(max_dist) %>%
  define_cv_grid(cv_fold_size)

# Get crossvalidation fold/grid info
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
focal_vs_comp <- focal_vs_comp %>%
  mutate(species = factor(species))

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

# Study results
ggplot(focal_vs_comp, aes(growth, growth_hat, alpha = 0.2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red", size = 1) +
  facet_wrap(~species, nrow = 2) +
  coord_fixed()

# Study results
focal_vs_comp <- focal_vs_comp %>%
  mutate(
    error = growth - growth_hat,
    error_bin = cut_number(error, n = 5)
    )

# Spatial patterns in residuals
ggplot(focal_vs_comp, aes(x = x, y, col = error)) +
  geom_point(size = 0.4) +
  coord_fixed(ratio = 1) +
  scale_color_gradient2(low="blue", mid="white", high="red") +
  theme_bw() +
  facet_wrap(~species, nrow = 2) +
  labs(title = "Spatial distribution of residuals (continuous scale)")


ggplot(focal_vs_comp, aes(x = x, y, col = error_bin)) +
  geom_point(size = 0.4) +
  coord_fixed(ratio = 1) +
  scale_colour_brewer(palette = "RdBu") +
  theme_bw() +
  facet_wrap(~species, nrow = 2) +
  labs(title = "Spatial distribution of residuals (binned)")




# Prior parameters for sigma2:
a_0 <- 250
b_0 <- 25

# Prior parameters for betas and lambdas:
mu_0 <- rep(0, length(posterior_param$mu_star)) %>%
  matrix(ncol = 1)
V_0 <- length(posterior_param$mu_star) %>% diag()

sigma_2_mu_0 <- b_0/(a_0-1)
sigma_2_var_0 <- b_0^2 / ((a_0 - 1)^2 * (a_0 - 2))


# Resulting prior mean, variance, and distribution of betas & lambdas:
nu_0 <- 2*a_0
Sigma_0 <- (b_0/a_0)*V_0

# Resulting posterior mean, variance, and distribution of betas & lambdas:
nu_star <- 2*posterior_param$a_star
Sigma_star <- (posterior_param$b_star/posterior_param$a_star)*posterior_param$V_star

beta_lambda_posterior_df <-
  rmvt(10000, sigma = Sigma_star, mu = as.vector(posterior_param$mu_star), df = nu_star) %>%
  data.frame() %>%
  as_tibble() %>%
  purrr::set_names(colnames(posterior_param$V_star)) %>%
  tidyr::gather(type, value)

# Plot all prior distributions:
posterior_distributions <- beta_lambda_posterior_df


ggplot(posterior_distributions, aes(x=value)) +
  geom_density(size = 0.75) +
  facet_wrap(~type, scales = "free", ncol = 6) +
  labs(title = "Posterior distributions") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw()

