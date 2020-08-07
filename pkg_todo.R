# Load packages ----------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
library(forestecology)
library(snakecase)
library(skimr)
suppressPackageStartupMessages(library(sf))
library(sfheaders)
# devtools::install_github("rvalavi/blockCV")
library(blockCV)
library(tictoc)
library(yardstick)
library(viridis)
library(mvnfast)
library(ggridges)





# Load & preprocess data -------------------------------------------------------
# Append additional species data
bw_census_2008 <- bw_census_2008 %>%
  left_join(bw_species, by = "sp") %>%
  select(-c(genus, species, latin))

# Maybe?
# - Add lat/long to all coordinates? Function? Based on Dave comments on Slack




# Compute growth ---------------------------------------------------------------
census_2008 <- bw_census_2008
# we need to filter out the resprouts
census_2014 <- bw_census_2014 %>%
  filter(!str_detect(codes, "R"))

# How to designate unique identifier?
id <- "treeID"

bw_growth_df <-
  # Merge both censuses and compute growth:
  compute_growth(census_2008, census_2014, id) %>%
  mutate(sp = to_any_case(sp)) %>%
  # Convert data frame to sf object
  st_as_sf(coords = c("gx", "gy")) %>%
  # drop stemID
  select(-stemID)





# Add buffers ---------------------------------------------------------------
# This number acts as buffer size, but also determining neighbors
max_dist <- 7.5

# Add buffer variable to data frame
bw_growth_df <- bw_growth_df %>%
  add_buffer_variable(direction = "in", size = max_dist, region = bw_study_region)

ggplot() +
  geom_sf(data = bw_growth_df, aes(col = buffer))





# Define spatial CV folds ------------------------------------------------------
#
# Make this a function?
# Inputs
# - main_df
# - some mechanism to define grid. Refer Roberts
#
# Outputs
# - main_df with fold variable as factor

cv_fold_size <- 100

set.seed(76)
#
# Create reprex for posting on GitHub issues to get grid tagged on
#
bw_cv_grid <- spatialBlock(
  speciesData = bw_growth_df, theRange = 100, verbose = FALSE,
  # Some guess work in figuring this out:
  k = 28, xOffset = 0.5, yOffset = 0
)

# Add foldID to data
bw_growth_df <- bw_growth_df %>%
  mutate(foldID = bw_cv_grid$foldID)

# Visualize grid. Why does fold 19 repeat?
bw_cv_grid$plots +
  geom_sf(data = bw_growth_df %>% sample_frac(0.2), aes(col=factor(foldID)), size = 0.1)

# Remove weird folds with no trees in them from viz above
bw_growth_df <- bw_growth_df %>%
  filter(!foldID %in% c(19, 23, 21, 17, 8, 19)) %>%
  mutate(foldID = as.character(foldID))

# Deliverable
ggplot() +
  geom_sf(data = bw_growth_df, aes(col = foldID, alpha = buffer))





# Model ------------------------------------------------------------------------
tic()
focal_vs_comp_bw <- bw_growth_df %>%
  create_focal_vs_comp(max_dist = max_dist, species_notion = "sp", cv_grid = bw_cv_grid, id = "treeID")
toc()


model_formula_bw <- focal_vs_comp_bw$focal_notion_of_species %>%
  unique() %>%
  sort() %>%
  paste(., "*sp", sep = "", collapse = " + ") %>%
  paste("growth ~ sp + dbh + dbh*sp + ", .)  %>%
  as.formula()


# Fit model (compute posterior parameters)
posterior_param_bw <- focal_vs_comp_bw %>%
  fit_bayesian_model(model_formula = model_formula_bw, run_shuffle = FALSE, prior_hyperparameters = NULL)

# Plot results
species_list_bw <- focal_vs_comp_bw$focal_notion_of_species %>% levels()
species_list_bw

posterior_plots <- plot_posterior_parameters(posterior_param = posterior_param_bw, species_list = species_list_bw)

posterior_plots[["beta_0"]]
posterior_plots[["beta_dbh"]]
posterior_plots[["lambdas"]]

# Make predictions, compute and save RMSE, and reset
predictions <- focal_vs_comp_bw %>%
  predict_bayesian_model(model_formula = model_formula_bw, posterior_param = bw_fit_model) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))

predictions %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)


# Cross-validation
tic()
cv_bw <- focal_vs_comp_bw %>%
  run_cv(model_formula = model_formula_bw, max_dist = max_dist, cv_grid = bw_cv_grid) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))

cv_bw %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
toc()

















# Number of permutation shuffles:
num_shuffle <- 4

# Compute observed RMSE for all models, but only do permutation shuffling for
# models 3,6,9
model_numbers <- c(3)
species_notion_vector <- c("trait_group", "family", "species")

# Save results here
run_time <- rep(0, length(species_notion_vector))
observed_RMSE <- rep(0, length(species_notion_vector))
observed_RMSE_CV <- rep(0, length(species_notion_vector))
shuffle_RMSE <- vector("list", length(species_notion_vector))
shuffle_RMSE_CV <- vector("list", length(species_notion_vector))
filename <- str_c(format(Sys.time(), "%Y-%m-%d"), "_model_comp_tbl_", num_shuffle, "_shuffles.RData")

for(i in 1:length(species_notion_vector)){
  # Start clock
  tic()

  # Modeling and species stuff
  species_notion <- species_notion_vector[i]
  bw_specs <- bw_growth_df %>%
    get_model_specs(model_number = 3, species_notion = "sp")

  # Focal vs comp main dataframe for analysis
  focal_vs_comp_bw <- bw_growth_df_orig %>%
    create_focal_vs_comp(max_dist, model_specs = bw_specs, cv_grid = bw_cv_grid, id = "treeID")


  # 1. Compute observed test statistic: RMSE with no cross-validation ----
  # Fit model (compute posterior parameters)
  bw_fit_model <- focal_vs_comp_bw %>%
    fit_bayesian_model(model_specs = bw_specs)

  # Make predictions, compute and save RMSE, and reset
  observed_RMSE[i] <- focal_vs_comp_bw %>%
    predict_bayesian_model(model_specs = bw_specs, posterior_param = bw_fit_model) %>%
    right_join(bw_growth_df_orig, by = c("focal_ID" = "treeID")) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)


  # 2. Compute observed test statistic: RMSE with cross-validation ----
  observed_RMSE_CV[i] <- focal_vs_comp_bw %>%
    run_cv(model_specs = bw_specs, max_dist = max_dist, cv_grid = bw_cv_grid) %>%
    right_join(bw_growth_df_orig, by = c("focal_ID" = "treeID")) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)


  # 3. Permutation distribution: RMSE with no cross-validation ----
  # Only do permutation shuffling for models 3, 6, 9
  # Compute num_shuffle permutation test statistics
  shuffle_RMSE[[i]] <- numeric(length = num_shuffle)

  for(j in 1:num_shuffle){
    # Fit model (compute posterior parameters) with shuffling
    bw_fit_model_shuffle <- focal_vs_comp_bw %>%
      fit_bayesian_model(model_specs = bw_specs, run_shuffle = TRUE)

    # Make predictions, compute and save RMSE, and reset
    shuffle_RMSE[[i]][j] <- focal_vs_comp_bw %>%
      predict_bayesian_model(model_specs = bw_specs, posterior_param = bw_fit_model_shuffle) %>%
      right_join(bw_growth_df_orig, by = c("focal_ID" = "treeID")) %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate)
  }


  # 4. Permutation distribution: RMSE with cross-validation ----
  # Compute num_shuffle permutation test statistics
  shuffle_RMSE_CV[[i]] <- numeric(length = num_shuffle)

  # Compute num_shuffle permutation test statistics
  for(j in 1:num_shuffle){
    # Compute and save RMSE, and reset
    shuffle_RMSE_CV[[i]][j] <- focal_vs_comp_bw %>%
      run_cv(model_specs = bw_specs, max_dist = max_dist, cv_grid = bw_cv_grid, run_shuffle = TRUE) %>%
      right_join(bw_growth_df_orig, by = c("focal_ID" = "treeID")) %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate)

    # Status update
    str_c("notion of species: ", species_notion_vector[i], ", shuffle with permutation ", j) %>% print()
  }


  # 5. Stop clock
  clock <- toc(quiet = TRUE)
  run_time[i] <- clock$toc - clock$tic


  # 6. Save
  model_comp_tbl <- tibble(
    species_notion = species_notion_vector,
    run_time = run_time,
    observed_RMSE = observed_RMSE,
    observed_RMSE_CV = observed_RMSE_CV,
    shuffle_RMSE = shuffle_RMSE,
    shuffle_RMSE_CV = shuffle_RMSE_CV,
  )
  save(model_comp_tbl, file = filename)
}








load("2020-06-25_model_comp_tbl_49_shuffles.RData")
model_comp <- bind_rows(
  model_comp_tbl %>% select(species_notion, run_time, observed = observed_RMSE, shuffle = shuffle_RMSE) %>% mutate(CV = FALSE),
  model_comp_tbl %>% select(species_notion, run_time, observed = observed_RMSE_CV, shuffle = shuffle_RMSE_CV) %>% mutate(CV = TRUE)
) %>%
  gather(type, RMSE, -c(species_notion, run_time, CV)) %>%
  mutate(
    species_notion = case_when(
      species_notion == "trait_group" ~ "1. Trait-based (6): lambda = 6 x 6",
      species_notion == "family" ~ "2. Phylogenetic family (20): lambda = 20 x 20",
      species_notion == "species" ~ "3. Actual species (36): lambda = 36 x 36"
    )
  ) %>%
  filter(species_notion != "3. Actual species (36): lambda = 36 x 36")

model_comp_observed <- model_comp %>%
  filter(type == "observed") %>%
  unnest(cols = c(RMSE))
model_comp_shuffle <- model_comp %>%
  filter(type == "shuffle") %>%
  unnest(cols = c(RMSE))

xlab <- expression(paste('RMSE (cm ',y^{-1},')'))

ggplot() +
  geom_vline(data = model_comp_observed, aes(xintercept = RMSE, col = CV), linetype = "dashed", show.legend = F) +
  geom_histogram(data = model_comp_shuffle, aes(x = RMSE, fill = CV), bins = 200) +
  labs(fill = "Cross-validated?", x = xlab) +
  facet_wrap(~species_notion, ncol = 1) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)
