library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(viridis)
library(forestecology)
library(blockCV)
library(tictoc)


# Compute growth of trees based on census data ---------------------------------
census_2013_scbi <- here("paper/scbi.stem2.csv") %>%
  read_csv() %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh)/10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

census_2018_scbi <- here("paper/scbi.stem3.csv") %>%
  read_csv() %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh)/10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

growth_scbi <-
  compute_growth(
    census_1 = census_2013_scbi,
    census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
    id = "stemID"
  )



# Add spatial information -----------------------------------------------------
# Define buffer region using competitive distance range
comp_dist <- 7.5

study_region_scbi <- tibble(
  x = c(0, 300, 300, 0, 0),
  y = c(300, 300, 600, 600, 300)
) %>%
  sf_polygon()

growth_scbi <- growth_scbi %>%
  add_buffer_variable(size = comp_dist, region = study_region_scbi)

# Manually define spatial blocks to act as folds
fold1 <- rbind(c(0, 300), c(150, 300), c(150, 450), c(0, 450))
fold2 <- rbind(c(150, 300), c(300, 300), c(300, 450), c(150, 450))
fold3 <- rbind(c(0, 450), c(150, 450), c(150, 600), c(0, 600))
fold4 <- rbind(c(150, 450), c(300, 450), c(300, 600), c(150, 600))
n_fold <- 4

blocks_scbi <- bind_rows(
  sf_polygon(fold1), sf_polygon(fold2), sf_polygon(fold3), sf_polygon(fold4)
) %>%
  mutate(folds = c(1:n_fold) %>% factor())

# Associate each observation to a fold
SpatialBlock_scbi <- spatialBlock(
  speciesData = growth_scbi, k = n_fold, selection = "systematic",
  blocks = blocks_scbi, showBlocks = FALSE, verbose = FALSE
)

growth_scbi <- growth_scbi %>%
  mutate(foldID = SpatialBlock_scbi$foldID %>% factor())



# Compute focal versus competitor tree information -----------------------------
focal_vs_comp_scbi <- growth_scbi %>%
  create_focal_vs_comp(comp_dist, blocks = blocks_scbi, id = "stemID")



# Fit model and make predictions -----------------------------------------------
# Number of permutation shuffles:
num_shuffle <- 2


# Save results here
run_time <- 0
observed_RMSE <- 0
observed_RMSE_CV <- 0
shuffle_RMSE <- vector("list", 1)
shuffle_RMSE_CV <- vector("list", 1)
filename <- here("paper/simulation_results/") %>%
  str_c(format(Sys.time(), "%Y-%m-%d"), "_scbi_", num_shuffle, "_shuffles")


# Run all simulations
# 0. Setup simulation for this species type ----
# Start clock
tic()


# 1. Compute observed test statistic: RMSE with no cross-validation ----
# Fit model (compute posterior parameters)
comp_bayes_lm_scbi <- focal_vs_comp_scbi %>%
  comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)

# Make predictions and compute RMSE
observed_RMSE <- focal_vs_comp_scbi %>%
  mutate(growth_hat = predict(comp_bayes_lm_scbi, focal_vs_comp_scbi)) %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)


# 2. Compute observed test statistic: RMSE with cross-validation ----
observed_RMSE_CV <- focal_vs_comp_scbi %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_scbi) %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)


# 3. Permutation distribution: RMSE with no cross-validation ----
# Compute num_shuffle permutation test statistics
shuffle_RMSE <- numeric(length = num_shuffle)
for(j in 1:num_shuffle){
  # Fit model (compute posterior parameters) with shuffling
  comp_bayes_lm_scbi <- focal_vs_comp_scbi %>%
    comp_bayes_lm(prior_param = NULL, run_shuffle = TRUE)

  # Make predictions and compute RMSE
  shuffle_RMSE[j] <-  focal_vs_comp_scbi %>%
    mutate(growth_hat = predict(comp_bayes_lm_scbi, focal_vs_comp_scbi)) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)
}


# 4. Permutation distribution: RMSE with cross-validation ----
# Compute num_shuffle permutation test statistics
shuffle_RMSE_CV <- numeric(length = num_shuffle)

# Compute num_shuffle permutation test statistics
for(j in 1:num_shuffle){
  # Compute and save RMSE
  shuffle_RMSE_CV[j] <- focal_vs_comp_scbi %>%
    run_cv(comp_dist = comp_dist, blocks = blocks_scbi, run_shuffle = TRUE) %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)

  # Status update
  str_c("Shuffle with permutation ", j, " at ", Sys.time()) %>% print()
}


# 5. Save results ----
clock <- toc(quiet = TRUE)
run_time <- clock$toc - clock$tic

model_comp_tbl <- tibble(
  run_time = run_time,
  observed_RMSE = observed_RMSE,
  observed_RMSE_CV = observed_RMSE_CV,
  shuffle_RMSE = shuffle_RMSE,
  shuffle_RMSE_CV = shuffle_RMSE_CV,
)
save(model_comp_tbl, file = filename %>% str_c(".RData"))




# Visualize results ------------------------------------------------------------
# Load results and plot
str_c(filename, ".RData") %>% load()

model_comp <- bind_rows(
  model_comp_tbl %>% select(run_time, observed = observed_RMSE, shuffle = shuffle_RMSE) %>% mutate(CV = FALSE),
  model_comp_tbl %>% select(run_time, observed = observed_RMSE_CV, shuffle = shuffle_RMSE_CV) %>% mutate(CV = TRUE)
) %>%
  gather(type, RMSE, -c(run_time, CV))

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
