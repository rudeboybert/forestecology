
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
# devtools::install_github("rvalavi/blockCV")
library(blockCV)
```

### Load & preprocess data

We first load and preprocess the data corresponding to the two census
reflecting the time period over which we consider growth in dbh. We load
them into R as “tibble” data frames thereby ensuring a standardized
input/output format that can be used across all `tidyverse` packages
@tidyverse. Furthermore, we ensure that the different variables have the
correct names, types (`dbl`, `data`, `factor`).

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

We then combine the two census data frames into a single data frame that
now includes a numerical variable `growth` reflecting the average annual
growth of dbh in cm. Furthermore, variables that (in theory) remain
unchanged between censuses appear only once, such as `gx`, `gy`,
species-related variables. Variables that should change between censuses
are tagged with `1/2` indicating earlier/later, such as `dbh1/dbh2`,
`codes1/codes2`.

The resulting data frames are named with some variation of `growth_df`.

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

\*\*EDA of both BigWoods & SCBI\*: Note the large variation in growths
for the SCBI trees over the BigWoods trees.

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

### Add spatial information

We now add spatial information to the `growth_df` data frame.
Specifically:

1.  In order to control for study region “edge effects” (cite Waller?),
    we need a function that adds “buffers” of trees. In our case, since
    our model of interspecies competition relies on a spatial definition
    of who is a “neighboring competitor” and certain explanatory
    varibles such as biomass are cummulative, we need to ensure that all
    trees being modeled are not biased to have different neighbor
    structure, in particular the trees at the boundary of study regions.
2.  Assign each tree to a “fold” for cross-validation purposes.
    Conventional cross-validation schemes assign observations to folds
    by resampling individual observations at random. However, underlying
    this scheme is an assumption that the observations are independent.
    In the case of forest census data, observations exhibit spatial
    autocorrelation, and thus this dependence must be incorporated in
    our resampling scheme @roberts2017 @pohjankukka2017. Packages that
    have implemented spatial cross-validation include @valavi2019

Thus, before we can incorporate the above information to `growth_df`, we
need to define two constants:

``` r
cv_fold_size <- 100
max_dist <- 7.5
```

For a focal tree of interest, the `max_dist` of 7.5 meters acts as a
radius defining a neighborhood within which all trees are considered
competitors. In other words, all trees within 7.5 meters of the focal
tree are considered competitor trees. Other studies have estimated this
distance; we used 7.5 meters as an average of estimated values
\[@canham2004, @canham2006, @uriarte2004, @tatsumi2013\]. Following
Tobler’s first law of geography that “everything is related to
everything else, but near things are more related than distant things.”
@tobler1970firstlawofgeo, we assume that the degree of spatial
autocorrelation is inversely-related to distance. However, we further
assume that once trees are more than 7.5 meters apart, this
autocorrelation is negligeable.

Using the [`blockCV`](https://github.com/rvalavi/blockCV) package, in
particular the example code in the package
[vignette](http://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/vignettes/BlockCV_for_SDM.html),
we

  - Define the blocking structure
  - Determine the degree of spatial autocorrelation for various
    `max_dist` values

<!-- end list -->

``` r
# spatial blocking by specified range with random assignment
```

  - If the above doesn’t work, the go back to what we used for PLOS
    paper.
  - Consider making `growth_df` an `sf` object

**Big Woods**:

**SCBI**:
