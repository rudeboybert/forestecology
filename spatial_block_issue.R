library(tidyverse)
library(forestecology)
library(snakecase)
library(blockCV)
library(lubridate)
library(sf)
library(sfheaders)

# Read in census files
data(census_df1_ex, census_df2_ex)

# Filter out resprouts
census_df2_ex_no_r <- census_df2_ex %>%
  filter(!str_detect(codes, 'R'))

id <- 'ID'

ex_growth_df <-
  # Merge both censuses and compute growth:
  compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
  mutate(
    sp = to_any_case(sp),
    sp = as.factor(sp)) %>%
  # drop stemID
  select(-ID)

data(ex_study_region)

# set max dist
max_dist <- 1

# add buffer
ex_growth_df <- ex_growth_df %>%
  add_buffer_variable(direction = "in", size = max_dist, region = ex_study_region)

ggplot() +
  geom_sf(data = ex_growth_df, aes(col = buffer), size = 2)

ex_cv_grid <- spatialBlock(
  speciesData = ex_growth_df, theRange = 5, verbose = FALSE, k = 2
)




# Bert's solution: Manually create a blocks sf object
fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
blocks <- bind_rows(
  sf_polygon(fold1), sf_polygon(fold2)
) %>%
  mutate(foldID = c(1, 2))

# Plot
ggplot() +
  geom_sf(data = ex_growth_df, aes(col = buffer), size = 2) +
  geom_sf(data = blocks, fill = "transparent")

# fit spatialBlock()
ex_cv_grid <- spatialBlock(
  speciesData = ex_growth_df,
  verbose = FALSE,
  k = 2,
  # Note new arguments
  selection = "predefined",
  blocks = blocks,
  foldsCol = "foldID"
)

# Add foldID to data
ex_growth_df <- ex_growth_df %>%
  mutate(foldID = ex_cv_grid$foldID %>% as.factor())

# Visualize grid
ex_cv_grid$plots +
  geom_sf(data = ex_growth_df, aes(col = foldID), size = 2)

# Deliverable
ggplot() +
  geom_sf(data = ex_growth_df, aes(col = foldID, shape = buffer))

# No need for this since we already defined sf object blocks
# ex_cv_grid_sf <- ex_cv_grid$blocks %>%
#   st_as_sf()
#
# ex_cv_grid_sf
