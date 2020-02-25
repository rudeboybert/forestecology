
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
library(sf)
library(sfheaders)
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
  mutate(spcode = to_any_case(spcode)) %>%
  select(
    treeID = treeid, stemID = stemtag, sp = spcode, quadrat, gx, gy, dbh, 
    date, codes
  )
bw_2014 <- 
  "https://deepblue.lib.umich.edu/data/downloads/1831ck00f" %>% 
  read_delim(delim = "\t") %>% 
  mutate(spcode = to_any_case(spcode)) %>%
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
  select(sp = spcode, genus, species, latin, family, trait_group)

bw_2008 <- bw_2008 %>%
  left_join(bw_species,by='sp')
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
# we need to filter out the resprouts
census_df2 <- bw_2014 %>% filter(!str_detect(codes,'R'))
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
  compute_growth(census_df1, census_df2, id) %>%
  # they are mesuaring in mm while we are measuring in cm!!!
  mutate(growth = growth/10)
```

``` r
growth_df <- bind_rows(
  bw_growth_df %>% select(growth) %>% mutate(site = "bw"),
  scbi_growth_df %>% select(growth) %>% mutate(site = "scbi")
)
ggplot(growth_df, aes(x = growth, y = ..density.., fill = site)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.05) +
  labs(x = "Average annual growth in dbh (cm per yr)") +
  coord_cartesian(xlim = c(-0.5, 1))
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

#### Defining buffers

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

**Example of buffer at work**:

``` r
# Boundary polygon
square_boundary <- tibble(
  x = c(0,0,1,1),
  y = c(0,1,1,0)
) %>% 
  sf_polygon()

# Buffer polygon
square_buffer <- square_boundary %>% 
  st_buffer(dist = -0.1)

ggplot() +
  geom_sf(data = square_boundary) +
  geom_sf(data = square_buffer, col="red") 
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

**Big Woods**:

``` r
# Bigwoods study region boundary polygon
bw_boundary <- bigwoods_study_region %>% 
  sf_polygon()

# Buffer polygon
bw_buffer <- bw_boundary %>%
  st_buffer(dist = -max_dist)

# Convert data frame to sf object
bw_growth_df <- bw_growth_df %>% 
  st_as_sf(coords = c("gx", "gy"))

# ID which points are in buffer and which are not
index <- st_intersects(bw_growth_df, bw_buffer, sparse = FALSE)
bw_growth_df <- bw_growth_df %>% 
  mutate(buffer = index)

# Plot
ggplot() +
  geom_sf(data = bw_boundary) +
  geom_sf(data = bw_buffer, col="red") +
  # Only random sample of 1000 trees:
  geom_sf(data = bw_growth_df %>% sample_n(1000), aes(col=buffer), size = 0.5)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

**SCBI**:

``` r
# Bigwoods study region boundary polygon
scbi_boundary <- tibble(x = c(0,400,400,0,0), y = c(0,0,640,640,0)) %>% 
  sf_polygon()

# Buffer polygon
scbi_buffer <- scbi_boundary %>%
  st_buffer(dist = -max_dist)

# Convert data frame to sf object
scbi_growth_df <- scbi_growth_df %>% 
  st_as_sf(coords = c("gx", "gy"))

# ID which points are in buffer and which are not
index <- st_intersects(scbi_growth_df, scbi_buffer, sparse = FALSE)
scbi_growth_df <- scbi_growth_df %>% 
  mutate(buffer = index)

# Plot
ggplot() +
  geom_sf(data = scbi_boundary) +
  geom_sf(data = scbi_buffer, col="red") +
  # Only random sample of 1000 trees:
  geom_sf(data = scbi_growth_df %>% sample_n(1000), aes(col=buffer), size = 0.5)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

#### Spatial cross-validation

We use the [`blockCV`](https://github.com/rvalavi/blockCV) package to
define the spatial grid, whose elements will act as the folds in our
leave-one-out (by “one” we meen “one grid block”) cross-validation
scheme.

**Big Woods**:

``` r
bw_cv_grid <- spatialBlock(
  speciesData = bw_growth_df, theRange = 100, k = 28, 
  xOffset = 0.5, yOffset = 0.99999,
  verbose = FALSE
)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

``` r

# Add foldID to data
bw_growth_df <- bw_growth_df %>% 
  mutate(
    foldID = bw_cv_grid$foldID,
    foldID = factor(foldID)
  )

# Visualize grid
bw_cv_grid$plots +
  geom_sf(data = bw_growth_df, aes(col=foldID), size = 0.1)
```

<img src="man/figures/README-unnamed-chunk-13-2.png" width="100%" />

**SCBI**:

``` r
scbi_cv_grid <- spatialBlock(
  speciesData = scbi_growth_df, theRange = 100, k = 28, yOffset = 0.6, verbose = FALSE
)
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

``` r

# Add foldID to data
scbi_growth_df <- scbi_growth_df %>% 
  mutate(
    foldID = scbi_cv_grid$foldID,
    foldID = factor(foldID)
  )

# Visualize grid
scbi_cv_grid$plots +
  geom_sf(data = scbi_growth_df, aes(col=foldID), size = 0.1)
```

<img src="man/figures/README-unnamed-chunk-14-2.png" width="100%" />

### Model specification

Next we specify the growth model we want. We will model growth as a
function of tree identiy, size, and the size and identiy of its
neighbors. The model can be run on different grouping of individuals
(e.g., based on species or trait groupings). It can also be run with
different measures of competition.

  - `model_number = 1`: No competition growth only depends on `dbh` and
    individual grouping.
  - `model_number = 2`: Competition but identity of competitor does not
    matter, just sum of competitior biomass.
  - `model_number = 3`: Full model with competition and competitor
    identity.

<!-- end list -->

``` r
bw_specs <- get_model_specs(bw_2008, 3, 'trait_group')

bw_specs
#> $model_formula
#> growth ~ trait_group + dbh + dbh * trait_group + biomass + biomass * 
#>     trait_group + evergreen * trait_group + maple * trait_group + 
#>     misc * trait_group + oak * trait_group + short_tree * trait_group + 
#>     shrub * trait_group
#> <environment: 0x7fba060b7f60>
#> 
#> $notion_of_focal_species
#> [1] "trait_group"
#> 
#> $notion_of_competitor_species
#> [1] "trait_group"
#> 
#> $species_of_interest
#> [1] "oak"        "evergreen"  "maple"      "shrub"      "short_tree"
#> [6] "misc"
scbi_specs <- get_model_specs(scbi_2013, 3, 'sp')
```

  - `get_model_specs()`. Inputs:
      - define identification/grouping classification
      - `growth_df` format
  - `create_focal_and_comp()`. Inputs:
      - `growth_df`
      - `max_dist`: who are your competitors
      - `model_specs`: grouping of competitor biomasses

### Model fit and prediction

  - `fit_bayesian_model()`.
      - Inputs: `focal_and_comp`, `model_specs`
      - Output: `posterior_param`
  - `predict_bayesian_model()`.
      - Inputs: `focal_and_comp`, `model_specs`, `posterior_param`
      - Output: `focal_and_comp` with updated `growth_hat` variable
  - Show observed vs predicted growth
  - CV: `run_cv()` is a wrapper to `fit_bayesian_model()` and
    `predict_bayesian_model()`.
  - Permutation test:
      - Add line to that shuffles competitor species within
        `focal_and_comp` just before fitting model
      - permutation AND CV: add `run_shuffle` argument to `run_cv()`

### Model output and performance

  - Make plot posterior parameter functions
