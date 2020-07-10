library(tidyverse)
library(usethis)
library(snakecase)
library(sf)
library(sfheaders)



# Boundary polygon of bigwoods region
bigwoods_study_region <-
  tibble(
    # Study region boundary
    x = c(-100, -100, -300, -300, -200, -200, 300, 300, 400, 400, 500, 500, -100),
    y = c(0, 100, 100, 200, 200, 400, 400, 200, 200, 100, 100, 0, 0)
  ) %>%
  # Convert to sf object
  sf_polygon()
use_data(bigwoods_study_region, overwrite = TRUE)



# Species info
families <- "data-raw/species_list.csv" %>%
  read_csv() %>%
  # Select necessary variables
  select(spcode, family, traitclust2) %>%
  # Differences in spelling
  mutate(
    spcode = case_when(
      spcode == "Black/Red Oak Hybrid" ~ "Black/Red Oak hybrid",
      spcode == "Black/Northern Pin Hybrid" ~ "Black/Northern Pin hybrid",
      spcode == "Highbush Blueberry" ~ "highbush blueberry",
      TRUE ~ spcode
    ),
    trait_group = traitclust2
  ) %>%
  select(-c(traitclust2)) %>%
  # Convert all to snake_case
  mutate(
    spcode = to_any_case(spcode),
    family = to_any_case(family),
    trait_group = to_any_case(trait_group)
  )
use_data(families, overwrite = TRUE)



# Import bigwoods data
bigwoods <- "data-raw/BigWoods2015_new.csv" %>%
  read_csv() %>%
  # Remove initially non-necessary variables
  select(
    -starts_with("not new"), -starts_with("notes"),
    -c(quad, row, xsample, side, ysample, tag, stem)
  ) %>%
  # Note readr package reads blanks as NA's. Convert these to blank to ""
  mutate(
    code08 = ifelse(is.na(code08), "", code08),
    code14 = ifelse(is.na(code14), "", code14)
  ) %>%
  # Define dbh and growth variables
  mutate(
    dbh03 = gbh03 / pi,
    dbh08 = gbh08 / pi,
    dbh14 = gbh14 / pi,
    growth = (dbh14 - dbh08) / (2014 - year_08)
  ) %>%
  # Remove those trees with year_08 = NA, i.e. were only added in 2014
  filter(!is.na(year_08)) %>%
  # Convert species to snake_case
  mutate(species = to_any_case(species)) %>%
  # Join with families data. If no matched family_philo or trait_groud found,
  # assign to "Misc"
  left_join(families, by = c("species" = "spcode")) %>%
  mutate(
    family_phylo = ifelse(is.na(family_phylo), "Misc", family_phylo),
    trait_group = ifelse(is.na(trait_group), "Misc", trait_group),
  ) %>%
  # Convert all species info to factors b/c of issue of rare levels not being
  # included in training set, but then appearing in test set.
  mutate(
    species = factor(species),
    family_phylo = factor(family_phylo),
    trait_group = factor(trait_group)
  ) %>%
  # Remove 32 + 113 = 145 of 50178 that aren't strictly in interior of study
  # region boundary polygon b/c of issues with forming crossvalidation grid
  # later
  mutate(inside_blocks = point.in.polygon(x, y, bigwoods_study_region$x, bigwoods_study_region$y)) %>%
  filter(inside_blocks == 1) %>%
  # Add ID variable
  rownames_to_column(var = "ID") %>%
  # Clean up variables
  select(
    ID, species, family_phylo, trait_group, x, y, growth, dbh08, dbh14,
    code14
  )
use_data(bigwoods, overwrite = TRUE)
