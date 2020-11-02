library(tidyverse)
library(usethis)
library(snakecase)
library(sf)
library(sfheaders)
library(forestecology)
library(lubridate)
library(blockCV)



# Boundary polygon of bigwoods region ------------------------------------------
bw_study_region <-
  tibble(
    # Study region boundary
    x = c(-100, -100, -300, -300, -200, -200, 300, 300, 400, 400, 500, 500, -100),
    y = c(0, 100, 100, 200, 200, 400, 400, 200, 200, 100, 100, 0, 0)
  ) %>%
  # Convert to sf object
  sf_polygon()
use_data(bw_study_region, overwrite = TRUE)



# Species info -----------------------------------------------------------------
# Get trait_cluster variable for all bigwood species
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
    # Rename:
    trait_group = traitclust2
  ) %>%
  mutate_at(c("spcode", "family", "trait_group"), to_any_case) %>%
  select(spcode, family, trait_group)

bw_species <-
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
use_data(bw_species, overwrite = TRUE)



# Import bigwoods data
bw_census_2008 <-
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )
use_data(bw_census_2008, overwrite = TRUE)

bw_census_2014 <-
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )
use_data(bw_census_2014, overwrite = TRUE)

# Small example for whole workflow
census_df1_ex <- tibble(
  ID = 1:10,
  sp = rep(c('sugar maple', 'American beech'),5),
  gx = c(0.75, 1.5, 1.75, 3, 3.25, 5.5, 8, 8.5, 8.75, 8.75),
  gy = c(2.5, 2.5, 2.25, 1.5, 1.75, 4.5, 1.5, 0.75, 1.5, 1.75),
  date = ymd('20150601'),
  codes = 'M',
  dbh = c(5, 20, 15, 12, 35, 6, 22, 14, 42, 4)
)
use_data(census_df1_ex, overwrite = TRUE)


census_df2_ex <- tibble(
  ID = c(1:9,11,12),
  sp = c(rep(c('sugar maple', 'American beech'),4),'sugar maple','sugar maple','sugar maple'),
  gx = c(0.75, 1.5, 1.75, 3, 3.25, 5.5, 8, 8.5, 8.75, 6.5, 2.5),
  gy = c(2.5, 2.5, 2.25, 1.5, 1.75, 4.5, 1.5, 0.75, 1.5, 3, 4.5),
  date = ymd('20200601'),
  codes = c(rep('M',5),'R',rep('M',5)),
  dbh = c(6, 24, 20, 14, 42, 2, 25, 19, 49, 2, 2)
)
use_data(census_df2_ex, overwrite = TRUE)

ex_study_region <-
  tibble(
    # Study region boundary
    x = c(0,10,10,0,0),
    y = c(0,0,5,5,0)
  ) %>%
  # Convert to sf object
  sf_polygon()
use_data(ex_study_region, overwrite = TRUE)

# Make all intermediate steps of small example for clearer examples
ex_growth_df <-
  compute_growth(census_df1_ex, census_df2_ex  %>% filter(!str_detect(codes, 'R')), "ID") %>%
  mutate(
    sp = to_any_case(sp),
    sp = as.factor(sp))
use_data(ex_growth_df, overwrite = TRUE)

ex_growth_df_spatial  <- ex_growth_df %>%
  add_buffer_variable(direction = "in", size = 1, region = ex_study_region)

fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
blocks <- bind_rows(
  sf_polygon(fold1),
  sf_polygon(fold2) ) %>%
  mutate(foldID = c(1, 2))

ex_cv_grid <- spatialBlock(
  speciesData = ex_growth_df,
  verbose = FALSE,
  k = 2,
  selection = "systematic",
  blocks = blocks)

# Add foldID to data
ex_growth_df_spatial <- ex_growth_df_spatial %>%
  mutate(foldID = ex_cv_grid$foldID %>% as.factor())
use_data(ex_growth_df_spatial, overwrite = TRUE)

ex_cv_grid_sf <- ex_cv_grid$blocks %>%
  st_as_sf()
use_data(ex_cv_grid_sf, overwrite = TRUE)

focal_vs_comp_ex <- ex_growth_df_spatial %>%
  create_focal_vs_comp(1, cv_grid_sf = ex_cv_grid_sf, id = "ID")
use_data(focal_vs_comp_ex, overwrite = TRUE)


# fit the model
posterior_param_ex <- focal_vs_comp_ex %>%
  fit_bayesian_model()
use_data(posterior_param_ex, overwrite = TRUE)





# Example growth_df data frame used to illustrate focal_vs_comp()
growth_df_ex <- tibble(
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
use_data(growth_df_ex, overwrite = TRUE)








