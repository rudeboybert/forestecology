library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(viridis)
library(forestecology)
library(blockCV)
library(tictoc)


# Compute growth of trees based on census data ---------------------------------
census_2008_bw <- census_2008_bw %>%
  left_join(species_bw, by = "sp") %>%
  select(-c(genus, species, latin))

growth_bw <-
  compute_growth(
    census_1 = census_2008_bw %>%
      mutate(sp = to_any_case(sp)),
    census_2 = census_2014_bw %>%
      filter(!str_detect(codes, "R")) %>%
      mutate(sp = to_any_case(sp)),
    id = "treeID"
  )



# Add spatial information -----------------------------------------------------
comp_dist <- 7.5
cv_block_size <- 100
data(study_region_bw)

## Add buffer ----
growth_bw <- growth_bw %>%
  add_buffer_variable(direction = "in", size = comp_dist, region = study_region_bw)

ggplot() +
  geom_sf(data = growth_bw %>% sample_frac(0.2), aes(col = buffer), size = 0.5)


## Add spatial folds ----
set.seed(76)
bw_spatialBlock <- spatialBlock(
  speciesData = growth_bw, theRange = cv_block_size, k = 28, xOffset = 0.5,
  yOffset = 0, verbose = FALSE, showBlocks = FALSE
)
growth_bw <- growth_bw %>%
  mutate(foldID = bw_spatialBlock$foldID)

# Visualize grid. Why does fold 19 repeat?
ggplot() +
  geom_sf(data = bw_spatialBlock$blocks %>% st_as_sf()) +
  geom_sf(data = growth_bw %>% sample_frac(0.2),
          aes(col = factor(foldID)), size = 0.1, show.legend = FALSE) +
  geom_sf_text(data = bw_spatialBlock$blocks %>% st_as_sf(),
               aes(label = folds))

growth_bw <- growth_bw %>%
  filter(!foldID %in% c(19, 23, 21, 17, 8, 19)) %>%
  mutate(foldID = factor(foldID))

blocks_bw <- bw_spatialBlock$blocks %>%
  st_as_sf()



# Compute focal versus competitor tree information -----------------------------
focal_vs_comp_bw <- growth_bw %>%
  create_focal_vs_comp(comp_dist, blocks = blocks_bw, id = "treeID")



# Fit model and make predictions -----------------------------------------------
# Number of permutation shuffles:
num_shuffle <- 49

# For all three possible notions of species
species_notion_vector <- c("trait_group", "family", "sp")

# Save results here
run_time <- rep(0, length(species_notion_vector))
observed_RMSE <- rep(0, length(species_notion_vector))
observed_RMSE_CV <- rep(0, length(species_notion_vector))
shuffle_RMSE <- vector("list", length(species_notion_vector))
shuffle_RMSE_CV <- vector("list", length(species_notion_vector))
filename <- here("etc/PLOSOne/") %>%
  str_c(format(Sys.time(), "%Y-%m-%d"), "_bw_", num_shuffle, "_shuffles")

# Run all simulations
for(i in 1:length(species_notion_vector)){
  # 0. Setup simulation for this species type ----
  # Start clock
  tic()

  # Focal vs comp main dataframe for analysis
  focal_vs_comp_bw <- growth_bw %>%
    mutate(sp = .data[[species_notion_vector[i]]]) %>%
    create_focal_vs_comp(comp_dist, blocks = blocks_bw, id = "treeID")


  # 1. Compute observed test statistic: RMSE with no cross-validation ----
  # Fit model (compute posterior parameters)
  comp_bayes_lm_bw <- focal_vs_comp_bw %>%
    comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)

  # Make predictions and compute RMSE
  observed_RMSE[i] <- focal_vs_comp_bw %>%
    mutate(growth_hat = predict(comp_bayes_lm_bw, focal_vs_comp_bw)) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)


  # 2. Compute observed test statistic: RMSE with cross-validation ----
  observed_RMSE_CV[i] <- focal_vs_comp_bw %>%
    run_cv(comp_dist = comp_dist, blocks = blocks_bw) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)


  # 3. Permutation distribution: RMSE with no cross-validation ----
  # Compute num_shuffle permutation test statistics
  shuffle_RMSE[[i]] <- numeric(length = num_shuffle)
  for(j in 1:num_shuffle){
    # Fit model (compute posterior parameters) with shuffling
    comp_bayes_lm_bw <- focal_vs_comp_bw %>%
      comp_bayes_lm(prior_param = NULL, run_shuffle = TRUE)

    # Make predictions and compute RMSE
    shuffle_RMSE[[i]][j] <-  focal_vs_comp_bw %>%
      mutate(growth_hat = predict(comp_bayes_lm_bw, focal_vs_comp_bw)) %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate)
  }


  # 4. Permutation distribution: RMSE with cross-validation ----
  # Compute num_shuffle permutation test statistics
  shuffle_RMSE_CV[[i]] <- numeric(length = num_shuffle)

  # Compute num_shuffle permutation test statistics
  for(j in 1:num_shuffle){
    # Compute and save RMSE
    shuffle_RMSE_CV[[i]][j] <- focal_vs_comp_bw %>%
      run_cv(comp_dist = comp_dist, blocks = blocks_bw, run_shuffle = TRUE) %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate)

    # Status update
    str_c("notion of species: ", species_notion_vector[i], ", shuffle with permutation ", j, " at ", Sys.time()) %>% print()
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



# Visualize results ------------------------------------------------------------
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
      species_notion == "sp" ~ "3. Actual species (36): lambda = 36 x 36"
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
  scale_fill_viridis(discrete = TRUE) +
  theme_light()
cv_plot

filename %>%
  str_c(".png") %>%
  ggsave(plot = cv_plot)













# Residual analysis ------------------------------------------------------------
ggplot(focal_vs_comp_scbi, aes(x = growth, y = growth_hat)) +
  geom_point(size = 0.5, color = rgb(0, 0, 0, 0.25)) +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  labs(
    x = "Observed growth in DBH", y = "Predicted growth in DBH",
    title = "Predicted vs Observed Growth"
  )



focal_vs_comp_scbi %>%
  st_as_sf() %>%
  # TODO: Need to investigate missingness
  filter(!is.na(growth_hat)) %>%
  mutate(
    error = growth - growth_hat,
    error_bin = cut_number(error, n = 5),
    error_compress = ifelse(error < -0.75, -0.75, ifelse(error > 0.75, 0.75, error))
  ) %>%
  ggplot() +
  geom_sf(aes(col = error_compress), size = 1) +
  theme_bw() +
  scale_color_gradient2(
    low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf",
    name = expression(paste("Residual (cm ", y^{-1}, ")")),
    breaks = seq(from = -0.75, to = 0.75, by = 0.25),
    labels = c("< -0.75", "-0.5", "0.25", "0", "0.25", "0.5", "> 0.75")
  ) +
  labs(x = "Meter", y = "Meter")
