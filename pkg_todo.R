# Load packages ----------------------------------------------------------------
library(tidyverse)
library(forestecology)
library(snakecase)
library(skimr)
library(sf)
library(sfheaders)
# devtools::install_github("rvalavi/blockCV")
library(blockCV)
library(tictoc)
library(yardstick)
library(viridis)





# Load & preprocess data -------------------------------------------------------
#
# Include in data/
#
# Read in census data from 2008
bw_2008 <-
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )

#
# Include in data/
#
# Read in census data from 2014
bw_2014 <-
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )

#
# Include in data/
#
# Read in grouping classification data
bw_species <-
  "https://deepblue.lib.umich.edu/data/downloads/000000086" %>%
  read_delim(delim = "\t") %>%
  # convert all to snake case:
  mutate_at(c("species", "genus", "family", "idlevel", "spcode"), to_any_case) %>%
  # join trait group
  left_join(families, by = c("spcode", "family")) %>%
  mutate(
    sp = str_sub(genus, 1, 2),
    sp = str_c(sp, str_sub(species, 1, 2)),
    sp = tolower(sp),
    latin = str_c(genus, species, sep = " "),
    latin = to_any_case(latin)
  ) %>%
  select(sp = spcode, genus, species, latin, family, trait_group)

# bw_2008 <- bw_2008 %>%
#   left_join(bw_species,by='sp')

#
# Include in data/
# As sf object or tibble?
#
# bigwoods_study_region


#
# Maybes?
# - Add lat/long to all coordinates? Function? Based on Dave comments
# - rename dbh1 & dbh2 to





# Compute growth ---------------------------------------------------------------
census_df1 <- bw_2008
# we need to filter out the resprouts
census_df2 <- bw_2014 %>%
  filter(!str_detect(codes, 'R'))

#
# How to designate unique identifier?
#
id <- "treeID"

bw_growth_df <-
  # Merge both censuses and compute growth:
  compute_growth(census_df1, census_df2, id) %>%
  mutate(sp = to_any_case(sp)) %>%
  # Convert data frame to sf object
  st_as_sf(coords = c("gx", "gy")) %>%
  #
  # drop stemID
  #
  select(-stemID)





# Add buffers ---------------------------------------------------------------

# This number acts as buffer, but also determining neighbors
max_dist <- 7.5

# Study region boundary polygon
bw_buffer_region <- bigwoods_study_region %>% sf_polygon() %>%
  compute_buffer_region(direction = "in", size = max_dist)

# Deliverable
ggplot() +
  geom_sf(data = bigwoods_study_region %>% sf_polygon()) +
  geom_sf(data = bw_buffer_region, col = "orange")


# Dave makes attempt at this function in R/spatial.R
# DA: Okay I think this works!
bw_growth_df <- bw_growth_df %>%
  define_buffer(size = max_dist, region = bigwoods_study_region %>% sf_polygon())

# Deliverable
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

# Visualize grid again
bw_cv_grid$blocks %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf()


# Remove weird folds with no trees in them from viz above
bw_growth_df <- bw_growth_df %>%
  filter(!foldID %in% c(19, 23, 21, 17, 8, 19)) %>%
  mutate(foldID = as.character(foldID))

# Deliverable
ggplot() +
  geom_sf(data = bw_growth_df, aes(col = foldID, alpha = buffer))




# Model ------------------------------------------------------------------------

# Run it once
bw_specs <- bw_growth_df %>%
  get_model_specs(model_number = 3, species_notion = "sp")


tic()
focal_vs_comp_bw <- bw_growth_df %>%
  create_focal_vs_comp(max_dist, model_specs = bw_specs, cv_grid = bw_cv_grid, id = "treeID")
toc()


tic()
focal_vs_comp_bw_2 <- bw_growth_df %>%
  create_focal_vs_comp_2(max_dist = max_dist, species_notion = "sp", cv_grid = bw_cv_grid, id = "treeID")
toc()

x <- focal_vs_comp_bw %>%
  group_by(focal_ID) %>%
  summarize(biomass = sum(comp_basal_area))

y <- focal_vs_comp_bw_2 %>%
  group_by(focal_ID) %>%
  summarize(biomass_2 = sum(comp_basal_area))

x %>%
  left_join(y, by = "focal_ID") %>%
  ggplot(aes(x = biomass, y = biomass_2)) +
  geom_point()





x <- focal_vs_comp_bw %>%
  filter(focal_ID == 1147) %>%
  pull(comp_ID)

y <- focal_vs_comp_bw_2 %>%
  filter(focal_ID == 1147) %>%
  pull(comp_ID)

new_trees <- setdiff(y, x)

temp <- bw_growth_df %>%
  filter(treeID %in% c(1147, y)) %>%
  mutate(
    focal = treeID == 1147,
    new = treeID %in% new_trees
  )

ggplot() +
  geom_sf(data = temp, aes(size = focal, col = new))



plot_title <- str_c("fold ", all_folds[i], ": Small black dots = competitor, cyan dots = test set, orange dots = buffer")

ggplot() +
  #geom_sf(data = bigwoods_study_region %>% sf_polygon(), col = "black") +
  geom_sf(data = current_fold_competitor_boundary, col = "red") +
  geom_sf(data = current_fold_boundary, col = "black") +
  geom_sf(data = growth_df_competitor_trees_current_fold, col = "orange", size = 3) +
  geom_sf(data = growth_df_focal_trees_current_fold, col = "cyan", size = 0.5) +
  geom_sf(data = temp, aes(size = focal, col = new)) +
  labs(title = plot_title)



bw_cv_grid$blocks %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = temp, aes(size = focal, col = new)) +
  coord_sf(xlim = c(75, 125), ylim = c(175, 225))






# 1. Compute observed test statistic: RMSE with no cross-validation ----
# Fit model (compute posterior parameters)
bw_fit_model <- focal_vs_comp_bw %>%
  fit_bayesian_model(model_specs = bw_specs, run_shuffle = FALSE)

# Make predictions, compute and save RMSE, and reset
predictions <- focal_vs_comp_bw %>%
  predict_bayesian_model(model_specs = bw_specs, posterior_param = bw_fit_model)





%>%
  right_join(bw_growth_df_orig, by = c("focal_ID" = "treeID")) %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)












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
