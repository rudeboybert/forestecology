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
  mutate(
    sp = to_any_case(sp),
    #
    # sp has to be a factor so that no errors occur in cross-validation
    #
    sp = as.factor(sp),
    species = sp,
    family = as.factor(family),
    trait_group = as.factor(trait_group)
  ) %>%
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
# Waiting to response on https://github.com/rvalavi/blockCV/issues/14
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

bw_cv_grid_sf <- bw_cv_grid$blocks %>%
  st_as_sf()





# Bayesian modeling ------------------------------------------------------------
focal_vs_comp_bw <- bw_growth_df %>%
  create_focal_vs_comp(max_dist = max_dist, cv_grid_sf = bw_cv_grid_sf, id = "treeID")

# a) Fit model (compute posterior parameters) with no permutation shuffling
posterior_param_bw <- focal_vs_comp_bw %>%
  fit_bayesian_model(prior_param = NULL, run_shuffle = FALSE)

# a) Make predictions and compute RMSE
predictions <- focal_vs_comp_bw %>%
  predict_bayesian_model(posterior_param = posterior_param_bw) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
predictions %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)


if(FALSE){
  # b) Fit model (compute posterior parameters) with permutation shuffling
  posterior_param_bw <- focal_vs_comp_bw %>%
    fit_bayesian_model(prior_param = NULL, run_shuffle = TRUE)

  # b) Make predictions and compute RMSE
  predictions <- focal_vs_comp_bw %>%
    predict_bayesian_model(posterior_param = posterior_param_bw) %>%
    right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
  predictions %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)
}





# Plot posterior parameters ----------------------------------------------------
species_list_bw <- focal_vs_comp_bw$focal_sp %>% unique()

posterior_plots <- plot_posterior_parameters(posterior_param = posterior_param_bw, species_list = species_list_bw)
posterior_plots[["beta_0"]]
posterior_plots[["beta_dbh"]]
posterior_plots[["lambdas"]]





# Cross-validation -------------------------------------------------------------
tic()
cv_bw <- focal_vs_comp_bw %>%
  run_cv(max_dist = max_dist, cv_grid = bw_cv_grid) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
toc()

cv_bw %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)





# Recreate Fig2 from Allen (2020): Full cross-validation simualation -----------

# Number of permutation shuffles:
num_shuffle <- 9

# For all three possible notions of species
species_notion_vector <- c("trait_group", "family", "species")

# Save results here
run_time <- rep(0, length(species_notion_vector))
observed_RMSE <- rep(0, length(species_notion_vector))
observed_RMSE_CV <- rep(0, length(species_notion_vector))
shuffle_RMSE <- vector("list", length(species_notion_vector))
shuffle_RMSE_CV <- vector("list", length(species_notion_vector))
filename <- str_c("results/", format(Sys.time(), "%Y-%m-%d"), "_model_comp_tbl_", num_shuffle, "_shuffles")

# Run all simulations
for(i in 1:length(species_notion_vector)){
  # 0. Setup simulation for this species type ----
  # Start clock
  tic()

  # Focal vs comp main dataframe for analysis
  focal_vs_comp_bw <- bw_growth_df %>%
    mutate(sp = .data[[species_notion_vector[i]]]) %>%
    create_focal_vs_comp(max_dist = max_dist, cv_grid_sf = bw_cv_grid_sf, id = "treeID")


  # 1. Compute observed test statistic: RMSE with no cross-validation ----
  # Fit model (compute posterior parameters)
  posterior_param_bw <- focal_vs_comp_bw %>%
    fit_bayesian_model(prior_param = NULL, run_shuffle = FALSE)

  # Make predictions, compute and save RMSE
  observed_RMSE[i] <- focal_vs_comp_bw %>%
    predict_bayesian_model(posterior_param = posterior_param_bw) %>%
    right_join(bw_growth_df, by = c("focal_ID" = "treeID")) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)


  # 2. Compute observed test statistic: RMSE with cross-validation ----
  observed_RMSE_CV[i] <- focal_vs_comp_bw %>%
    run_cv(max_dist = max_dist, cv_grid = bw_cv_grid) %>%
    right_join(bw_growth_df, by = c("focal_ID" = "treeID")) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)


  # 3. Permutation distribution: RMSE with no cross-validation ----
  # Compute num_shuffle permutation test statistics
  shuffle_RMSE[[i]] <- numeric(length = num_shuffle)

  for(j in 1:num_shuffle){
    # Fit model (compute posterior parameters) with shuffling
    posterior_param_bw <- focal_vs_comp_bw %>%
      fit_bayesian_model(prior_param = NULL, run_shuffle = TRUE)

    # Make predictions, compute and save RMSE, and reset
    shuffle_RMSE[[i]][j] <-  focal_vs_comp_bw %>%
      predict_bayesian_model(posterior_param = posterior_param_bw) %>%
      right_join(bw_growth_df, by = c("focal_ID" = "treeID")) %>%
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
      run_cv(max_dist = max_dist, cv_grid = bw_cv_grid, run_shuffle = TRUE) %>%
      right_join(bw_growth_df, by = c("focal_ID" = "treeID")) %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate)

    # Status update
    str_c("notion of species: ", species_notion_vector[i], ", shuffle with permutation ", j) %>% print()
  }


  # 5. Save results ----
  clock <- toc(quiet = TRUE)
  run_time[i] <- clock$toc - clock$tic

  model_comp_tbl <- tibble(
    species_notion = species_notion_vector,
    run_time = run_time,
    observed_RMSE = observed_RMSE,
    observed_RMSE_CV = observed_RMSE_CV,
    shuffle_RMSE = shuffle_RMSE,
    shuffle_RMSE_CV = shuffle_RMSE_CV,
  )
  save(model_comp_tbl, file = filename %>% str_c(".RData"))
}


# Load results and plot
str_c(filename, ".RData") %>% load()

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
  )

model_comp_observed <- model_comp %>%
  filter(type == "observed") %>%
  unnest(cols = c(RMSE))
model_comp_shuffle <- model_comp %>%
  filter(type == "shuffle") %>%
  unnest(cols = c(RMSE))

cv_plot <- ggplot() +
  geom_vline(data = model_comp_observed, aes(xintercept = RMSE, col = CV), linetype = "dashed", show.legend = F) +
  geom_histogram(data = model_comp_shuffle, aes(x = RMSE, fill = CV), bins = 200) +
  labs(
    fill = "Cross-validated?",
    x = expression(paste("RMSE (cm ", y^{-1}, ")"))
  ) +
  facet_wrap(~species_notion, ncol = 1) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE)
cv_plot

filename %>%
  str_c(".png") %>%
  ggsave(plot = cv_plot)
