library(tidyverse)
library(forestecology)
library(snakecase)
library(blockCV)
library(lubridate)
library(sf)
library(sfheaders)

# Read in census files
data(census_1_ex, census_2_ex)

# Filter out resprouts
census_2_ex_no_r <- census_2_ex %>%
  filter(!str_detect(codes, 'R'))

id <- 'ID'

growth_ex <-
  # Merge both censuses and compute growth:
  compute_growth(census_1_ex, census_2_ex_no_r, id) %>%
  mutate(
    sp = to_any_case(sp),
    sp = as.factor(sp)) %>%
  # drop stemID
  select(-ID)

data(study_region_ex)

# set max dist
max_dist <- 1

# add buffer
growth_ex <- growth_ex %>%
  add_buffer_variable(direction = "in", size = max_dist, region = study_region_ex)

ggplot() +
  geom_sf(data = growth_ex, aes(col = buffer), size = 2)

#cv_grid_ex <- spatialBlock(
#  speciesData = growth_ex, theRange = 5, verbose = FALSE, k = 2
#)


# Bert's solution: Manually create a blocks sf object
fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
blocks <- bind_rows(
  sf_polygon(fold1),
  sf_polygon(fold2) ) %>%
  mutate(foldID = c(1, 2))


# Plot
ggplot() +
  geom_sf(data = growth_ex, aes(col = buffer), size = 2) +
  geom_sf(data = blocks, fill = "transparent")

# fit spatialBlock()
cv_grid_ex <- spatialBlock(
  speciesData = growth_ex,
  verbose = FALSE,
  k = 2,
  # Note new arguments
  selection = "systematic",
  blocks = blocks#,
 # foldsCol = "foldID"
)

# Add foldID to data
growth_ex <- growth_ex %>%
  mutate(foldID = cv_grid_ex$foldID %>% as.factor())

# Visualize grid
cv_grid_ex$plots +
  geom_sf(data = growth_ex, aes(col = foldID), size = 2)

# Deliverable
ggplot() +
  geom_sf(data = growth_ex, aes(col = foldID, shape = buffer))

