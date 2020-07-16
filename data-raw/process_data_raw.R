library(tidyverse)
library(usethis)
library(snakecase)
library(sf)
library(sfheaders)



# Boundary polygon of bigwoods region
bw_study_region <-
  tibble(
    # Study region boundary
    x = c(-100, -100, -300, -300, -200, -200, 300, 300, 400, 400, 500, 500, -100),
    y = c(0, 100, 100, 200, 200, 400, 400, 200, 200, 100, 100, 0, 0)
  ) %>%
  # Convert to sf object
  sf_polygon()
use_data(bw_study_region, overwrite = TRUE)


# Species info
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
