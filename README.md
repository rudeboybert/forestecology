
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forestecology

[![Travis Build
Status](https://travis-ci.org/rudeboybert/forestecology.svg?branch=master)](https://travis-ci.org/rudeboybert/forestecology)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/forestecology)](https://cran.r-project.org/package=forestecology)

## Installation

You can install the released version of forestecology from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("forestecology")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rudeboybert/forestecology")
```

## Example analysis

We present an example analysis using:

  - [Michigan Big Woods](https://doi.org/10.7302/wx55-kt18) research
    plot data
  - [Smithsonian Conservation Biology Institute (SCBI)
    ForestGEO](https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/tree/master/tree_main_census)
    research plot data

<!-- end list -->

``` r
library(tidyverse)
library(forestecology)
library(snakecase)
library(skimr)
```

### Load & preprocess data

**Big Woods**:

``` r
# Read in Big Woods census data from 2008 & 2014
bw_2008 <- 
  "https://deepblue.lib.umich.edu/data/downloads/z603qx485" %>% 
  read_delim(delim = "\t") %>% 
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh, 
    date, codes
  )
bw_2014 <- 
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>% 
  read_delim(delim = "\t") %>% 
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
  select(sp, spcode, genus, species, latin, family, trait_group)
```

**SCBI**:

``` r
scbi_2013 <- 
  "https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/census-csv-files/scbi.stem2.csv" %>% 
  read_csv() %>% 
  select(
    treeID, stemID, sp, quadrat, gx, gy, dbh, 
    date = ExactDate, codes, status
  )

scbi_2018 <-
  "https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/census-csv-files/scbi.stem3.csv" %>% 
  read_csv() %>% 
  select(
    treeID, stemID, sp, quadrat, gx, gy, dbh, 
    date = ExactDate, codes, status
  ) %>% 
  mutate(dbh = as.numeric(dbh))
```

### Compute growth

**Big Woods**:

``` r
census_df1 <- bw_2008
census_df2 <- bw_2014
id <- "treeID"

bw_growth_df <- 
  # Merge both censuses and compute growth:
  compute_growth(census_df1, census_df2, id) %>% 
  mutate(sp = to_any_case(sp)) %>% 
  left_join(bw_species, by = c("sp" = "spcode"))
```

**SCBI**:

``` r
census_df1 <- scbi_2013
census_df2 <- scbi_2018
id <- "stemID"

scbi_growth_df <- 
  # Merge both censuses and compute growth:
  compute_growth(census_df1, census_df2, id)
```

**Joint**:

``` r
growth_df <- bind_rows(
  bw_growth_df %>% select(growth) %>% mutate(site = "bw"),
  scbi_growth_df %>% select(growth) %>% mutate(site = "scbi")
)
ggplot(growth_df, aes(x = growth, y = ..density.., fill = site)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.5) +
  labs(x = "Average annual growth in dbh") +
  coord_cartesian(xlim = c(-5, 10))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />
