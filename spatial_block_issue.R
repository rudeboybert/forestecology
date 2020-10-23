library(tidyverse)
library(forestecology)
library(snakecase)
library(blockCV)
library(lubridate)

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
