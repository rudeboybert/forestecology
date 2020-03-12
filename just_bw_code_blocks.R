library(tidyverse)
library(forestecology)
library(snakecase)
library(skimr)
library(sf)
library(sfheaders)
library(blockCV)

bw_2008 <-
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh,
    date, codes
  )
bw_2014 <-
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh,
    date, codes
  )

# Read in bigwoods grouping classification data
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

bw_2008 <- bw_2008 %>%
  left_join(bw_species,by='sp')


census_df1 <- bw_2008
# we need to filter out the resprouts
census_df2 <- bw_2014 %>%
  filter(!str_detect(codes, 'R'))
id <- "treeID"

bw_growth_df <-
  # Merge both censuses and compute growth:
  compute_growth(census_df1, census_df2, id) %>%
  mutate(sp = to_any_case(sp))

cv_fold_size <- 100
max_dist <- 7.5

# Bigwoods study region boundary polygon
bw_boundary <- bigwoods_study_region %>%
  sf_polygon()

# Buffer polygon
bw_buffer <- bw_boundary %>%
  st_buffer(dist = -max_dist)

# Convert data frame to sf object
bw_growth_df <- bw_growth_df %>%
  st_as_sf(coords = c("gx", "gy"))

# ID which points are in buffer and which are not
buffer_index <- !st_intersects(bw_growth_df, bw_buffer, sparse = FALSE)
bw_growth_df <- bw_growth_df %>%
  mutate(buffer = as.vector(buffer_index))

# Plot
ggplot() +
  geom_sf(data = bw_boundary) +
  geom_sf(data = bw_buffer, col="red") +
  # Only random sample of 1000 trees:
  geom_sf(data = bw_growth_df %>% sample_n(1000), aes(col=buffer), size = 0.5)

set.seed(76)
bw_cv_grid <- spatialBlock(
  speciesData = bw_growth_df, theRange = 100, verbose = FALSE,
  # Some guess work in figuring this out:
  k = 28, xOffset = 0.5, yOffset = 0
)

# Add foldID to data
bw_growth_df <- bw_growth_df %>%
  mutate(
    foldID = bw_cv_grid$foldID
  )

# Visualize grid. Why does fold 19 repeat?
bw_cv_grid$plots +
  geom_sf(data = bw_growth_df %>% sample_frac(0.2), aes(col=factor(foldID)), size = 0.1)

# Remove weird folds with no trees in them from viz above
bw_growth_df <- bw_growth_df %>%
  filter(!foldID %in% c(19, 23, 21, 17, 8, 19))

bw_specs <- bw_growth_df %>%
  get_model_specs(model_number = 3, species_notion = 'trait_group')
bw_specs

focal_vs_comp_bw <- bw_growth_df %>%
  create_focal_vs_comp(max_dist, model_specs = bw_specs, cv_grid = bw_cv_grid, id = "treeID")

bw_fit_model <- focal_vs_comp_bw %>%
  fit_bayesian_model(model_specs = bw_specs)

bw_predict <- focal_vs_comp_bw %>%
  predict_bayesian_model(model_specs = bw_specs, posterior_param = bw_fit_model)

bw_predict %>%
  group_by(focal_ID) %>%
  summarise(growth = mean(growth), growth_hat = mean(growth_hat)) %>%
  ggplot(aes(growth, growth_hat)) +
  geom_point(size = 0.5, color = rgb(0,0,0,0.25)) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0)
