library(tidyverse)
library(usethis)
library(snakecase)
library(sf)
library(sfheaders)
library(forestecology)
library(lubridate)
library(blockCV)



# Big Woods data ----
## Boundary of study region ----
study_region_bw <-
  tibble(
    # Study region boundary
    x = c(-100, -100, -300, -300, -200, -200, 300, 300, 400, 400, 500, 500, -100),
    y = c(0, 100, 100, 200, 200, 400, 400, 200, 200, 100, 100, 0, 0)
  ) %>%
  # Convert to sf object
  sf_polygon()
use_data(study_region_bw, overwrite = TRUE)


## Species, family, and trait_cluster classification of all species
families <- "data-raw/species_list.csv" %>%
  read_csv() %>%
  mutate(
    # Differences in spelling
    spcode = case_when(
      spcode == "Black/Red Oak Hybrid" ~ "Black/Red Oak hybrid",
      spcode == "Black/Northern Pin Hybrid" ~ "Black/Northern Pin hybrid",
      spcode == "Highbush Blueberry" ~ "highbush blueberry",
      TRUE ~ spcode
    ),
    trait_group = traitclust2
  ) %>%
  mutate_at(c("spcode", "family", "trait_group"), to_any_case) %>%
  select(spcode, family, trait_group)

species_bw <-
  "https://deepblue.lib.umich.edu/data/downloads/000000086" %>%
  read_delim(delim = "\t") %>%
  # convert all to snake case:
  mutate_at(c("species", "genus", "family", "idlevel", "spcode"), to_any_case) %>%
  mutate(
    sp = str_sub(genus, 1, 2) %>% str_c(str_sub(species, 1, 2)) %>% tolower(),
    latin = str_c(genus, species, sep = " ") %>% to_any_case()
  ) %>%
  select(sp = spcode, genus, species, latin, family) %>%
  left_join(families, by = c("sp" = "spcode", "family"))
use_data(species_bw, overwrite = TRUE)


# Import census data
census_2008_bw <-
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )
use_data(census_2008_bw, overwrite = TRUE)

census_2014_bw <-
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )
use_data(census_2014_bw, overwrite = TRUE)



# Example data ----
## Boundary of study region ----
study_region_ex <-
  tibble(
    # Study region boundary
    x = c(0, 10, 10, 0, 0),
    y = c(0, 0, 5, 5, 0)
  ) %>%
  # Convert to sf object
  sf_polygon()
use_data(study_region_ex, overwrite = TRUE)


## Create census data ----
census_1_ex <- tibble(
  ID = 1:10,
  sp = rep(c("sugar maple", "American beech"), 5),
  gx = c(0.75, 1.5, 1.75, 3, 3.25, 5.5, 8, 8.5, 8.75, 8.75),
  gy = c(2.5, 2.5, 2.25, 1.5, 1.75, 4.5, 1.5, 0.75, 1.5, 1.75),
  date = ymd("20150601"),
  codes = "M",
  dbh = c(5, 20, 15, 12, 35, 6, 22, 14, 42, 4)
)
use_data(census_1_ex, overwrite = TRUE)

census_2_ex <- tibble(
  ID = c(1:9, 11, 12),
  sp = c(rep(c("sugar maple", "American beech"), 4), "sugar maple", "sugar maple", "sugar maple"),
  gx = c(0.75, 1.5, 1.75, 3, 3.25, 5.5, 8, 8.5, 8.75, 6.5, 2.5),
  gy = c(2.5, 2.5, 2.25, 1.5, 1.75, 4.5, 1.5, 0.75, 1.5, 3, 4.5),
  date = ymd("20200601"),
  codes = c(rep("M", 5), "R", rep("M", 5)),
  dbh = c(6, 24, 20, 14, 42, 2, 25, 19, 49, 2, 2)
)
use_data(census_2_ex, overwrite = TRUE)


## Create growth data frame ----
growth_ex <-
  compute_growth(
    census_1 = census_1_ex,
    census_2 = census_2_ex %>% filter(!str_detect(codes, "R")),
    id = "ID"
    ) %>%
  mutate(sp = to_any_case(sp) %>% factor())
use_data(growth_ex, overwrite = TRUE)


## Create growth with spatial info data frame ----
growth_spatial_ex <- growth_ex %>%
  add_buffer_variable(direction = "in", size = 1, region = study_region_ex)

# Manually create folds
fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
blocks <- bind_rows(
  sf_polygon(fold1),
  sf_polygon(fold2)
) %>%
  mutate(foldID = c(1, 2))

cv_grid_ex <- spatialBlock(
  speciesData = growth_ex,
  k = 2,
  selection = "systematic",
  blocks = blocks,
  verbose = FALSE
)

# Add foldID to data
growth_spatial_ex <- growth_spatial_ex %>%
  mutate(foldID = cv_grid_ex$foldID %>% as.factor())
use_data(growth_spatial_ex, overwrite = TRUE)


## Create spatial objects ----
cv_grid_sf_ex <- cv_grid_ex$blocks %>%
  st_as_sf()
use_data(cv_grid_sf_ex, overwrite = TRUE)


## Create focal_vs_comp data frame  ----
focal_vs_comp_ex <- growth_spatial_ex %>%
  create_focal_vs_comp(max_dist = 1, cv_grid_sf = cv_grid_sf_ex, id = "ID")
use_data(focal_vs_comp_ex, overwrite = TRUE)


## Fit model ----
comp_bayes_lm_ex <- focal_vs_comp_ex %>%
  comp_bayes_lm()
use_data(comp_bayes_lm_ex, overwrite = TRUE)




# Other data ----
## Create toy growth data frame used to illustrate focal_vs_comp() ----
growth_toy <- tibble(
  ID = 1:5,
  sp = c("tulip poplar", "red oak", "red oak", "tulip poplar", "tulip poplar"),
  gx = c(1, 1, 1, 4, 4),
  gy = c(4, 3, 2, 1, 2)
) %>%
  mutate(
    dbh1 = c(40, 25, 30, 35, 20),
    codes1 = rep("M", n()),
    codes2 = rep("M", n()),
    growth = c(1, 2, 1, 3, 2),
    dbh2 = dbh1 + growth,
    foldID = rep(1, n()) %>% as.character(),
    buffer = rep(FALSE, n())
  ) %>%
  # Convert data frame to sf object
  st_as_sf(coords = c("gx", "gy")) %>%
  select(ID, sp, dbh1, codes1, dbh2, codes2, growth, geometry, buffer, foldID)
use_data(growth_toy, overwrite = TRUE)
