
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
# install.packages("devtools")
devtools::install_github("rudeboybert/forestecology")
```

## Example analysis

We present an example analysis using the [Michigan Big Woods research
plot data](https://doi.org/10.7302/wx55-kt18) which is part of the
Smithsonian Institution’s [Forest Global Earth Observatory
(ForestGEO)](https://forestgeo.si.edu/) global network of forest
research sites.

Porting over code from `SCBI.R`

``` r
library(tidyverse)
library(forestecology)
library(snakecase)

# Read in Big Woods census and species data
bw_2008 <- read_delim("https://deepblue.lib.umich.edu/data/downloads/z603qx485", delim = "\t") %>% 
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh, 
    ExactDate = date, code_2008 = codes
  )
bw_2014 <- read_delim("https://deepblue.lib.umich.edu/data/downloads/1831ck00f", delim = "\t") %>% 
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh, 
    ExactDate = date, code_2014 = codes
  )
bw_species <- read_delim("https://deepblue.lib.umich.edu/data/downloads/000000086", delim = "\t") %>% 
  mutate(spcode = to_any_case(spcode)) %>% 
  left_join(families, by = "spcode") %>% 
  mutate(
    sp = str_sub(genus, 1, 2), 
    sp = str_c(sp, str_sub(species, 1, 2)),
    sp = tolower(sp),
    latin = str_c(genus, species, sep = " ")
  ) %>% 
  select(sp, genus, species, family, latin, trait_group)
```
