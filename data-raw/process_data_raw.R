library(tidyverse)
library(usethis)
library(snakecase)
library(sf)
library(sfheaders)



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
bw_censusdf1 <-
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )
use_data(bw_censusdf1, overwrite = TRUE)

bw_censusdf2 <-
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>%
  read_delim(delim = "\t") %>%
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, gx, gy, dbh,
    date, codes
  )
use_data(bw_censusdf2, overwrite = TRUE)
