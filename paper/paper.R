## ----setup, include = FALSE, cache = FALSE----------------------------
# Internally used packages
library(tidyverse)
library(conflicted)
library(viridis)
library(knitr)
library(here)

# knitr settings
opts_chunk$set(
  # Code output:
  warning = FALSE,
  message = FALSE,
  echo = TRUE,
  cache = TRUE,
  # Figure:
  out.width = "100%",
  fig.path = "figures/",
  fig.width = 16 / 2.5,
  fig.height = 9 / 2.5,
  fig.align = "center",
  fig.show = "hold",
  dpi = 600,
  # Etc:
  collapse = TRUE,
  comment = "##",
  tidy.opts = list(width.cutoff = 72),
  tidy = FALSE
)

# Random number generator seed value
set.seed(76)

# Set ggplot defaults output:
if (!is_html_output()) {
  # Grey theme:
  ggplot2::theme_set(theme_light())
  # Color scheme:
  # scale_colour_discrete <- scale_colour_viridis_d
}

# Set output width
options(
  width = 72,
  # Decimal precision
  digits = 3,
  # Number of rows for tibble printing
  tibble.print_max = 5,
  tibble.print_min = 5
)


## ----load-packages----------------------------------------------------
library(tidyverse)
library(lubridate)
library(sf)
library(patchwork)
library(forestecology)
library(blockCV)

# Resolve conflicting functions
filter <- dplyr::filter
select <- dplyr::select


## ----scbi-load-data---------------------------------------------------
census_2013_scbi <- read_csv("scbi.stem2.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    # Convert date from character to date
    date = mdy(date),
    # Convert dbh to be in cm
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

census_2018_scbi <- read_csv("scbi.stem3.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))


## ----scbi-compute-growth----------------------------------------------
growth_scbi <-
  compute_growth(
    census_1 = census_2013_scbi,
    census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
    id = "stemID"
  )
growth_scbi %>%
  select(stemID, sp, dbh1, dbh2, growth, geometry)


## ----scbi-trees, out.width="66%", fig.cap="Step 1 - Compute growth of trees based on census data. A map of the growth of a random sample of 500 trees from a 9 ha subsection of the Smithsonian Conservation Biology Institute (SCBI) forest plot."----
ggplot() +
  geom_sf(data = growth_scbi %>% sample_n(500), aes(size = growth)) +
  scale_size_binned(limits = c(0.1, 1)) +
  labs(size = expression(paste(Growth, " (cm ", y^{-1}, ")")))


## ----scbi-load-sp-data------------------------------------------------
sp_info <- read_csv("SCBI_ForestGEO_sp_ecology.csv") %>%
  select(
    sp = spcode, family, genus, species, canopy_position,
    drought_tolerance
  )
sp_info


## ---------------------------------------------------------------------
growth_scbi <- growth_scbi %>%
  left_join(sp_info, by = "sp") %>%
  mutate(sp = as.factor(sp))


## ----scbi-compute-comp-explanatory-variables--------------------------
# Install development version of allodb using:
# remotes::install_github("forestgeo/allodb")
library(allodb)
growth_scbi <- growth_scbi %>%
  mutate(
    # Compute basal area:
    basal_area = 0.0001 * pi * (dbh1 / 2)^2,
    # Compute above ground biomass:
    agb = get_biomass(
      dbh = dbh1,
      genus = genus,
      species = species,
      coords = c(-78.2, 38.9)
    )
  )


## ----add-buffer-------------------------------------------------------
# Define competitive distance range
comp_dist <- 7.5

# Manually construct study region boundary
study_region_scbi <- tibble(
  x = c(0, 300, 300, 0, 0),
  y = c(300, 300, 600, 600, 300)
) %>%
  sf_polygon()

growth_scbi <- growth_scbi %>%
  add_buffer_variable(size = comp_dist, region = study_region_scbi)


## ----manually-define-folds--------------------------------------------
# Manually define spatial blocks to act as folds
n_fold <- 4
fold1 <- cbind(c(0, 150, 150, 0), c(300, 300, 450, 450))
fold2 <- cbind(c(150, 300, 300, 150), c(300, 300, 450, 450))
fold3 <- cbind(c(0, 150, 150, 0), c(450, 450, 600, 600))
fold4 <- cbind(c(150, 300, 300, 150), c(450, 450, 600, 600))

blocks_scbi <- bind_rows(
  sf_polygon(fold1), sf_polygon(fold2),
  sf_polygon(fold3), sf_polygon(fold4)
) %>%
  mutate(folds = c(1:n_fold) %>% factor())

# Associate each observation to a fold
spatial_block_scbi <-
  spatialBlock(
    speciesData = growth_scbi, k = n_fold,
    selection = "systematic", blocks = blocks_scbi,
    showBlocks = FALSE, verbose = FALSE
  )

growth_scbi <- growth_scbi %>%
  mutate(foldID = spatial_block_scbi$foldID %>% factor())


## ----scbi-spatial-information, out.width="66%", fig.cap="Step 2 - Add spatial information. A buffer region and spatial cross-validation blocks 1 through 4. The location of each tree is marked with its fold number where the folds are delineated with solid lines. The color of each digit indicates whether the tree is part of the buffer region (thus will only be considered as a competitor tree) or is part of the interior of the study region (thus is a focal tree whose growth is of modeled interest)."----
ggplot() +
  geom_sf(
    data = blocks_scbi,
    fill = "transparent", linetype = "dashed"
  ) +
  geom_sf_text(
    data = growth_scbi %>% sample_n(1000),
    aes(label = foldID, col = buffer)
  )


## ----scbi-focal-vs-comp-----------------------------------------------
focal_vs_comp_scbi <- growth_scbi %>%
  create_focal_vs_comp(
    comp_dist, blocks = blocks_scbi, id = "stemID",
    comp_x_var = "basal_area"
  )

focal_vs_comp_scbi %>%
  select(focal_ID, focal_sp, geometry, growth, comp)


## ----internal, echo = FALSE, eval = FALSE-----------------------------
## # Internal: after removing trees in buffer and have NA for growth, there is one
## # tree that is not included in focal_vs_comp_scbi b/c it has no neighbors within
## # comp_dist = 7.5m of it
## growth_scbi %>%
##   filter(!buffer & !is.na(growth)) %>%
##   filter(!stemID %in% focal_vs_comp_scbi$focal_ID)
##
## growth_scbi %>%
##   mutate(flag = stemID == 18823) %>%
##   ggplot() +
##   geom_sf(aes(col = flag))


## ----demonstrate-unnest, scbi-focal-vs-comp-3-------------------------
focal_vs_comp_scbi %>%
  filter(focal_ID == 4) %>%
  select(focal_ID, dbh, comp) %>%
  unnest(cols = "comp")


## ----scbi-focal-vs-comp-map, out.width="66%", echo = FALSE, fig.cap="Step 3 - Identify all focal and corresponding competitor trees. The dashed circle extends 7.5m away from the focal tree 4 while all 20 competitor trees are within this circle.", fig.height=4----
comp_radius <- growth_scbi %>%
  filter(stemID == 4) %>%
  st_buffer(dist = comp_dist)

comp_radius_box <- growth_scbi %>%
  filter(stemID == 4) %>%
  st_buffer(dist = comp_dist + 10) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_intersects(growth_scbi, ., sparse = FALSE)

growth_scbi_subset <- growth_scbi %>%
  mutate(inside = comp_radius_box) %>%
  filter(inside)

ggplot() +
  geom_sf(data = growth_scbi_subset, aes(col = sp, size = dbh1)) +
  geom_sf(data = comp_radius, linetype = "dashed", alpha = 0, inherit.aes = FALSE) +
  coord_sf(xlim = st_bbox(comp_radius)[c("xmin", "xmax")], ylim = st_bbox(comp_radius)[c("ymin", "ymax")]) +
  geom_sf_text(data = growth_scbi %>% filter(stemID == 4), aes(label = stemID), size = 12) +
  labs(size = "DBH at first census", col = "species")


## ----scbi-model-fit---------------------------------------------------
comp_bayes_lm_scbi <- focal_vs_comp_scbi %>%
  comp_bayes_lm(prior_param = NULL)


## ----show-comp-bayes--------------------------------------------------
comp_bayes_lm_scbi


## ----scbi-model-predict-----------------------------------------------
focal_vs_comp_scbi <- focal_vs_comp_scbi %>%
  mutate(
    growth_hat = predict(comp_bayes_lm_scbi,
                         newdata = focal_vs_comp_scbi)
    )

## ----show-focal-vs-comp-----------------------------------------------
focal_vs_comp_scbi %>%
  select(focal_ID, focal_sp, dbh, growth, growth_hat)


## ----scbi-model-rmse--------------------------------------------------
model_rmse <- focal_vs_comp_scbi %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
model_rmse


## ----scbi-posterior-viz, out.width="100%", fig.cap= "Step 4 - Fit model. Posterior distributions of all parameters. For compactness we include only three species.", fig.width = 8, fig.height = 6----
# Plot posteriors for only a subset of species
sp_to_plot <- c("litu", "quru", "cagl")

plot1 <- autoplot(
  comp_bayes_lm_scbi,
  type = "intercepts",
  sp_to_plot = sp_to_plot
)
plot2 <- autoplot(
  comp_bayes_lm_scbi,
  type = "dbh_slopes",
  sp_to_plot = sp_to_plot
)
plot3 <- autoplot(
  comp_bayes_lm_scbi,
  type = "competition",
  sp_to_plot = sp_to_plot
)

# Combine plots using the patchwork package
(plot1 | plot2) / plot3


## ----scbi-permutation-model-fit---------------------------------------
comp_bayes_lm_scbi_shuffle <- focal_vs_comp_scbi %>%
  comp_bayes_lm(prior_param = NULL, run_shuffle = TRUE)

focal_vs_comp_scbi <- focal_vs_comp_scbi %>%
  mutate(
    growth_hat_shuffle = predict(comp_bayes_lm_scbi_shuffle,
                                 newdata = focal_vs_comp_scbi)
  )

## ----show-rmse-shuffle------------------------------------------------
model_rmse_shuffle <- focal_vs_comp_scbi %>%
  rmse(truth = growth, estimate = growth_hat_shuffle) %>%
  pull(.estimate)
model_rmse_shuffle


## ----scbi-spatial-cross-validation-schematic, out.width="66%", fig.cap= "Schematic of spatial cross-validation. Using the k = 1 fold (bottom-left) as the test set, k = 2 through 4 as the training set, along with a fold buffer extending outwards from the test set to maintain spatial independence between it and the training set.", echo = FALSE----
fold_number <- 1

test_fold_buffer <- blocks_scbi %>%
  filter(folds == fold_number)

test_vs_train <- focal_vs_comp_scbi %>%
  st_as_sf() %>%
  add_buffer_variable(
    direction = "out", size = comp_dist, region = test_fold_buffer
  ) %>%
  mutate(
    fold = case_when(
      foldID == fold_number ~ "test",
      foldID != fold_number & buffer ~ "training",
      foldID != fold_number & !buffer ~ "fold buffer"
    ),
    fold = factor(fold, levels = c("test", "fold buffer", "training"))
  )

ggplot() +
  geom_sf(data = compute_buffer_region(test_fold_buffer, direction = "out", size = comp_dist), col = "red", fill = "transparent") +
  geom_sf(data = blocks_scbi, fill = "transparent", linetype = "dashed") +
  geom_sf(data = test_vs_train %>% st_as_sf() %>% sample_n(1000), aes(col = fold, shape = fold))


## ----scbi-spatial-cv--------------------------------------------------
focal_vs_comp_scbi <- focal_vs_comp_scbi %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_scbi)

## ----show-rmse-cv-----------------------------------------------------
model_rmse_cv <- focal_vs_comp_scbi %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
model_rmse_cv


## ----scbi-simulation, out.width="100%", fig.cap="Comparison of root mean squared error of models for standard, permuted, and spatially cross-validated error estimates. The dotted lines show observed RMSE while the histograms show the null distribution of RMSE for 49 permutations under the null hypothesis of no competitor species identity effects. The colors indicate whether spatial cross-validation was used or not.", echo = FALSE----
include_graphics("simulation_results/2021-03-03_scbi_49_shuffles.tiff")


## ----appendix2-code---------------------------------------------------
census_2013_scbi <- read_csv("scbi.stem2.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 400, gy < 400)

census_2018_scbi <- read_csv("scbi.stem3.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 400, gy < 400)

growth_scbi <-
  compute_growth(
    census_1 = census_2013_scbi,
    census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
    id = "stemID"
  ) %>%
  # make all species the same, needs to be factor with at least
  # two levels
  mutate(sp = factor("A", levels = c("A", "B"))) %>%
  # Compute basal area:
  mutate(basal_area = 0.0001 * pi * (dbh1 / 2)^2)

study_region_scbi <- tibble(
  x = c(0, 400, 400, 0, 0),
  y = c(0, 0, 400, 400, 0)
) %>%
  sf_polygon()

n_fold <- 4
fold1 <- cbind(c(0, 200, 200, 0), c(0, 0, 200, 200))
fold2 <- cbind(c(200, 400, 400, 200), c(0, 0, 200, 200))
fold3 <- cbind(c(0, 200, 200, 0), c(200, 200, 400, 400))
fold4 <- cbind(c(200, 400, 400, 200), c(200, 200, 400, 400))

blocks_scbi <- bind_rows(
  sf_polygon(fold1), sf_polygon(fold2), sf_polygon(fold3),
  sf_polygon(fold4)
) %>%
  mutate(folds = c(1:n_fold) %>% factor())

# Associate each observation to a fold
spatial_block_scbi <-
  spatialBlock(
    speciesData = growth_scbi, k = n_fold,
    selection = "systematic", blocks = blocks_scbi,
    showBlocks = FALSE, verbose = FALSE
  )

growth_scbi <- growth_scbi %>%
  mutate(foldID = spatial_block_scbi$foldID %>% factor())

mult_dist_comp <- tibble(
  dist = c(5, 6.25, 7.5, 8.75, 10),
  rmse = 0
)

for (i in 1:length(mult_dist_comp$dist)) {
  comp_dist <- mult_dist_comp$dist[i]

  growth_scbi <- growth_scbi %>%
    add_buffer_variable(size = comp_dist, region = study_region_scbi)

  focal_vs_comp_scbi <- growth_scbi %>%
    create_focal_vs_comp(
      comp_dist = comp_dist, blocks = blocks_scbi, id = "stemID",
      comp_x_var = "basal_area"
    ) %>%
    run_cv(comp_dist = comp_dist, blocks = blocks_scbi)

  mult_dist_comp$rmse[i] <- focal_vs_comp_scbi %>%
    rmse(truth = growth, estimate = growth_hat) %>%
    pull(.estimate)
}


## ----appendix2-plot, out.width="100%", fig.cap="Cross-validated RMSE estimates for 5 competitive distances.", echo = FALSE----
mult_dist_comp %>%
  ggplot(aes(dist, rmse)) +
  geom_point() +
  xlab("Distance (m)") +
  ylab("RMSE (cm)")


## ----compar-pred-var--------------------------------------------------
census_2013_scbi <- read_csv("scbi.stem2.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

census_2018_scbi <- read_csv("scbi.stem3.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

growth_scbi <-
  compute_growth(
    census_1 = census_2013_scbi,
    census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
    id = "stemID"
  ) %>%
  left_join(sp_info, by = "sp") %>%
  mutate(
    sp = as.factor(sp),
    basal_area = 0.0001 * pi * (dbh1 / 2)^2,
    agb = get_biomass(
      dbh = dbh1,
      genus = genus,
      species = species,
      coords = c(-78.2, 38.9)
    )
  )

study_region_scbi <- tibble(
  x = c(0, 300, 300, 0, 0),
  y = c(300, 300, 600, 600, 300)
) %>%
  sf_polygon()

n_fold <- 4
fold1 <- cbind(c(0, 150, 150, 0), c(300, 300, 450, 450))
fold2 <- cbind(c(150, 300, 300, 150), c(300, 300, 450, 450))
fold3 <- cbind(c(0, 150, 150, 0), c(450, 450, 600, 600))
fold4 <- cbind(c(150, 300, 300, 150), c(450, 450, 600, 600))

blocks_scbi <- bind_rows(
  sf_polygon(fold1), sf_polygon(fold2), sf_polygon(fold3),
  sf_polygon(fold4)
) %>%
  mutate(folds = c(1:n_fold) %>% factor())

# Associate each observation to a fold
spatial_block_scbi <-
  spatialBlock(
    speciesData = growth_scbi, k = n_fold,
    selection = "systematic", blocks = blocks_scbi,
    showBlocks = FALSE, verbose = FALSE
  )

growth_scbi <- growth_scbi %>%
  mutate(foldID = spatial_block_scbi$foldID %>% factor())

comp_dist <- 7.5

growth_scbi <- growth_scbi %>%
  add_buffer_variable(size = comp_dist, region = study_region_scbi)

focal_vs_comp_ba <- growth_scbi %>%
  create_focal_vs_comp(
    comp_dist = comp_dist,
    blocks = blocks_scbi,
    id = "stemID",
    comp_x_var = "basal_area"
  ) %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_scbi)

focal_vs_comp_agb <- growth_scbi %>%
  create_focal_vs_comp(
    comp_dist = comp_dist,
    blocks = blocks_scbi,
    id = "stemID",
    comp_x_var = "agb"
  ) %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_scbi)

focal_vs_comp_ba %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)

focal_vs_comp_agb %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)


## ----compare-group-var------------------------------------------------
census_2013_scbi <- read_csv("scbi.stem2.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

census_2018_scbi <- read_csv("scbi.stem3.csv") %>%
  select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
  mutate(
    date = mdy(date),
    dbh = as.numeric(dbh) / 10
  ) %>%
  filter(gx < 300, between(gy, 300, 600))

growth_scbi_sp <-
  compute_growth(
    census_1 = census_2013_scbi,
    census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
    id = "stemID"
  ) %>%
  mutate(
    sp = as.factor(sp),
    basal_area = 0.0001 * pi * (dbh1 / 2)^2
  )

growth_scbi_can_pos <-
  compute_growth(
    census_1 = census_2013_scbi,
    census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
    id = "stemID"
  ) %>%
  left_join(sp_info, by = "sp") %>%
  mutate(
    canopy_position = str_replace(canopy_position, " ", "_"),
    canopy_position = str_replace(canopy_position, ",", ""),
    canopy_position = ifelse(is.na(canopy_position), "shrub_layer",
                             canopy_position),
    sp = as.factor(canopy_position),
    basal_area = 0.0001 * pi * (dbh1 / 2)^2
  )

study_region_scbi <- tibble(
  x = c(0, 300, 300, 0, 0),
  y = c(300, 300, 600, 600, 300)
) %>%
  sf_polygon()

n_fold <- 4
fold1 <- cbind(c(0, 150, 150, 0), c(300, 300, 450, 450))
fold2 <- cbind(c(150, 300, 300, 150), c(300, 300, 450, 450))
fold3 <- cbind(c(0, 150, 150, 0), c(450, 450, 600, 600))
fold4 <- cbind(c(150, 300, 300, 150), c(450, 450, 600, 600))

blocks_scbi <- bind_rows(
  sf_polygon(fold1), sf_polygon(fold2), sf_polygon(fold3),
  sf_polygon(fold4)
) %>%
  mutate(folds = c(1:n_fold) %>% factor())

# Associate each observation to a fold
spatial_block_scbi <-
  spatialBlock(
    speciesData = growth_scbi, k = n_fold,
    selection = "systematic", blocks = blocks_scbi,
    showBlocks = FALSE, verbose = FALSE
  )

growth_scbi_sp <- growth_scbi_sp %>%
  mutate(foldID = spatial_block_scbi$foldID %>% factor())
growth_scbi_can_pos <- growth_scbi_can_pos %>%
  mutate(foldID = spatial_block_scbi$foldID %>% factor())

comp_dist <- 7.5

growth_scbi_sp <- growth_scbi_sp %>%
  add_buffer_variable(size = comp_dist, region = study_region_scbi)
growth_scbi_can_pos <- growth_scbi_can_pos %>%
  add_buffer_variable(size = comp_dist, region = study_region_scbi)

focal_vs_comp_sp <- growth_scbi_sp %>%
  create_focal_vs_comp(
    comp_dist = comp_dist,
    blocks = blocks_scbi,
    id = "stemID",
    comp_x_var = "basal_area"
  ) %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_scbi)

focal_vs_comp_can_pos <- growth_scbi_can_pos %>%
  create_focal_vs_comp(
    comp_dist = comp_dist,
    blocks = blocks_scbi,
    id = "stemID",
    comp_x_var = "basal_area"
  ) %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_scbi)

focal_vs_comp_sp %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)

focal_vs_comp_can_pos %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)


## ----can-pos-comp-----------------------------------------------------
fit_mod_can_pos <- growth_scbi_can_pos %>%
  create_focal_vs_comp(
    comp_dist = comp_dist,
    blocks = blocks_scbi,
    id = "stemID",
    comp_x_var = "basal_area"
  ) %>%
  comp_bayes_lm(prior_param = NULL)


## ----can-pos-comp-plot, out.width="100%", fig.cap="Posterior distributions of all competition parameters.", echo = FALSE, fig.width = 16 / 2.3----
autoplot(fit_mod_can_pos, type = "competition")


## ----appendix1-code, eval=FALSE---------------------------------------
## library(tidyverse)
## library(lubridate)
## library(here)
## library(sf)
## library(viridis)
## library(forestecology)
## library(blockCV)
## library(tictoc)
##
##
## # Compute growth of trees based on census data ------------------------
## census_2013_scbi <- here("paper/scbi.stem2.csv") %>%
##   read_csv() %>%
##   select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
##   mutate(
##     date = mdy(date),
##     dbh = as.numeric(dbh) / 10
##   ) %>%
##   filter(gx < 300, between(gy, 300, 600))
##
## census_2018_scbi <- here("paper/scbi.stem3.csv") %>%
##   read_csv() %>%
##   select(stemID, sp, date = ExactDate, gx, gy, dbh, codes, status) %>%
##   mutate(
##     date = mdy(date),
##     dbh = as.numeric(dbh) / 10
##   ) %>%
##   filter(gx < 300, between(gy, 300, 600))
##
## growth_scbi <-
##   compute_growth(
##     census_1 = census_2013_scbi,
##     census_2 = census_2018_scbi %>% filter(!str_detect(codes, "R")),
##     id = "stemID"
##   ) %>%
##   # Compute basal area:
##   mutate(basal_area = 0.0001 * pi * (dbh1 / 2)^2)
##
##
## # Add spatial information ---------------------------------------------
## # Define buffer region using competitive distance range
## comp_dist <- 7.5
##
## study_region_scbi <- tibble(
##   x = c(0, 300, 300, 0, 0),
##   y = c(300, 300, 600, 600, 300)
## ) %>%
##   sf_polygon()
##
## growth_scbi <- growth_scbi %>%
##   add_buffer_variable(size = comp_dist, region = study_region_scbi)
##
## # Manually define spatial blocks to act as folds
## fold1 <- rbind(c(0, 300), c(150, 300), c(150, 450), c(0, 450))
## fold2 <- rbind(c(150, 300), c(300, 300), c(300, 450), c(150, 450))
## fold3 <- rbind(c(0, 450), c(150, 450), c(150, 600), c(0, 600))
## fold4 <- rbind(c(150, 450), c(300, 450), c(300, 600), c(150, 600))
## n_fold <- 4
##
## blocks_scbi <- bind_rows(
##   sf_polygon(fold1), sf_polygon(fold2),
##   sf_polygon(fold3), sf_polygon(fold4)
## ) %>%
##   mutate(folds = c(1:n_fold) %>% factor())
##
## # Associate each observation to a fold
## SpatialBlock_scbi <- spatialBlock(
##   speciesData = growth_scbi, k = n_fold, selection = "systematic",
##   blocks = blocks_scbi, showBlocks = FALSE, verbose = FALSE
## )
##
## growth_scbi <- growth_scbi %>%
##   mutate(foldID = SpatialBlock_scbi$foldID %>% factor())
##
## # Compute focal versus competitor tree information --------------------
## focal_vs_comp_scbi <- growth_scbi %>%
##   create_focal_vs_comp(
##     comp_dist, blocks = blocks_scbi, id = "stemID",
##     comp_x_var = "basal_area"
##   )
##
##
## # Fit model and make predictions --------------------------------------
## # Number of permutation shuffles:
## num_shuffle <- 49
##
## # Save results here
## run_time <- 0
## observed_RMSE <- 0
## observed_RMSE_CV <- 0
## shuffle_RMSE <- vector("list", 1)
## shuffle_RMSE_CV <- vector("list", 1)
## filename <- here("paper/simulation_results/") %>%
##   str_c("2021-03-03_scbi_", num_shuffle, "_shuffles")
##
## # Run all simulations
## # 0. Setup simulation for this species type ----
## # Start clock
## tic()
##
## # 1. Compute observed test statistic: RMSE with no cross-validation ---
## # Fit model (compute posterior parameters)
## comp_bayes_lm_scbi <- focal_vs_comp_scbi %>%
##   comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)
##
## # Make predictions and compute RMSE
## observed_RMSE <- focal_vs_comp_scbi %>%
##   mutate(
##     growth_hat =
##       predict(comp_bayes_lm_scbi, focal_vs_comp_scbi)
##   ) %>%
##   rmse(truth = growth, estimate = growth_hat) %>%
##   pull(.estimate)
##
## # 2. Compute observed test statistic: RMSE with cross-validation ------
## observed_RMSE_CV <- focal_vs_comp_scbi %>%
##   run_cv(comp_dist = comp_dist, blocks = blocks_scbi) %>%
##   rmse(truth = growth, estimate = growth_hat) %>%
##   pull(.estimate)
##
## # 3. Permutation distribution: RMSE with no cross-validation ----------
## # Compute num_shuffle permutation test statistics
## shuffle_RMSE <- numeric(length = num_shuffle)
## for (j in 1:num_shuffle) {
##   # Fit model (compute posterior parameters) with shuffling
##   comp_bayes_lm_scbi <- focal_vs_comp_scbi %>%
##     comp_bayes_lm(prior_param = NULL, run_shuffle = TRUE)
##
##   # Make predictions and compute RMSE
##   shuffle_RMSE[j] <- focal_vs_comp_scbi %>%
##     mutate(
##       growth_hat = predict(comp_bayes_lm_scbi, focal_vs_comp_scbi)
##     ) %>%
##     rmse(truth = growth, estimate = growth_hat) %>%
##     pull(.estimate)
## }
##
## # 4. Permutation distribution: RMSE with cross-validation -------------
## # Compute num_shuffle permutation test statistics
## shuffle_RMSE_CV <- numeric(length = num_shuffle)
##
## # Compute num_shuffle permutation test statistics
## for (j in 1:num_shuffle) {
##   # Compute and save RMSE
##   shuffle_RMSE_CV[j] <- focal_vs_comp_scbi %>%
##     run_cv(
##       comp_dist = comp_dist, blocks = blocks_scbi,
##       run_shuffle = TRUE
##     ) %>%
##     rmse(truth = growth, estimate = growth_hat) %>%
##     pull(.estimate)
##
##   # Status update
##   str_c("Shuffle with permutation ", j, " at ", Sys.time()) %>%
##     print()
## }
##
## # 5. Save results ----
## clock <- toc(quiet = TRUE)
## run_time <- clock$toc - clock$tic
##
## model_comp_tbl <- tibble(
##   run_time = run_time,
##   observed_RMSE = observed_RMSE,
##   observed_RMSE_CV = observed_RMSE_CV,
##   shuffle_RMSE = shuffle_RMSE,
##   shuffle_RMSE_CV = shuffle_RMSE_CV,
## )
## save(model_comp_tbl, file = filename %>% str_c(".RData"))
##
##
## # Visualize results ---------------------------------------------------
## model_comp <- bind_rows(
##   model_comp_tbl %>%
##     select(run_time,
##            observed = observed_RMSE,
##            shuffle = shuffle_RMSE
##     ) %>%
##     mutate(CV = FALSE),
##   model_comp_tbl %>%
##     select(run_time,
##            observed = observed_RMSE_CV,
##            shuffle = shuffle_RMSE_CV
##     ) %>%
##     mutate(CV = TRUE)
## ) %>%
##   gather(type, RMSE, -c(run_time, CV))
##
## model_comp_observed <- model_comp %>%
##   filter(type == "observed") %>%
##   unnest(cols = c(RMSE))
## model_comp_shuffle <- model_comp %>%
##   filter(type == "shuffle") %>%
##   unnest(cols = c(RMSE))
##
## cv_plot <- ggplot() +
##   geom_vline(
##     data = model_comp_observed,
##     aes(xintercept = RMSE, col = CV),
##     linetype = "dashed", show.legend = F
##   ) +
##   geom_histogram(
##     data = model_comp_shuffle,
##     aes(x = RMSE, fill = CV), bins = 50
##   ) +
##   labs(
##     fill = "Cross-validated?",
##     x = expression(paste("RMSE (cm ", y^{-1}, ")"))
##   ) +
##   scale_color_viridis(discrete = TRUE, option = "D") +
##   scale_fill_viridis(discrete = TRUE) +
##   theme_light()
## cv_plot
##
## filename %>%
##   str_c(".pdf") %>%
##   ggsave(plot = cv_plot, width = 16 / 2, height = 9 / 2)

