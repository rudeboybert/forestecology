
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
# remotes::install_github("rudeboybert/forestecology")
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
library(tictoc)

run_scbi <- FALSE
run_bw <- TRUE
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
census_df2 <- bw_2014 %>% 
  filter(!str_detect(codes, 'R'))
id <- "treeID"

bw_growth_df <- 
  # Merge both censuses and compute growth:
  compute_growth(census_df1, census_df2, id) %>% 
  mutate(sp = to_any_case(sp))
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

**Comparison**:

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
buffer_index <- !st_intersects(bw_growth_df, bw_buffer, sparse = FALSE)
bw_growth_df <- bw_growth_df %>% 
  mutate(buffer = as.vector(buffer_index))

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
buffer_index <- !st_intersects(scbi_growth_df, scbi_buffer, sparse = FALSE)
scbi_growth_df <- scbi_growth_df %>% 
  mutate(buffer = as.vector(buffer_index))

# Plot
ggplot() +
  geom_sf(data = scbi_boundary) +
  geom_sf(data = scbi_buffer, col="red") +
  # Only random sample of 1000 trees:
  geom_sf(data = scbi_growth_df %>% sample_n(1000), aes(col=buffer), size = 0.5)
```

#### Defining spatial cross-validation folds

We use the [`blockCV`](https://github.com/rvalavi/blockCV) package to
define the spatial grid, whose elements will act as the folds in our
leave-one-out (by “one” we meen “one grid block”) cross-validation
scheme.

**Big Woods**:

``` r
set.seed(76)
bw_cv_grid <- spatialBlock(
  speciesData = bw_growth_df, theRange = 100, verbose = FALSE,
  # Some guess work in figuring this out:
  k = 28, xOffset = 0.5, yOffset = 0
)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

``` r

# Add foldID to data
bw_growth_df <- bw_growth_df %>% 
  mutate(
    foldID = bw_cv_grid$foldID
  ) 

# Visualize grid. Why does fold 19 repeat?
bw_cv_grid$plots +
  geom_sf(data = bw_growth_df %>% sample_frac(0.2), aes(col=factor(foldID)), size = 0.1)
```

<img src="man/figures/README-unnamed-chunk-13-2.png" width="100%" />

``` r

# Remove weird folds with no trees in them from viz above
bw_growth_df <- bw_growth_df %>%
  filter(!foldID %in% c(19, 23, 21, 17, 8, 19))
```

**SCBI**:

``` r
scbi_cv_grid <- spatialBlock(
  speciesData = scbi_growth_df, theRange = 100, k = 28, yOffset = 0.9999, verbose = FALSE
)

# Add foldID to data
scbi_growth_df <- scbi_growth_df %>% 
  mutate(
    foldID = scbi_cv_grid$foldID
  )

# Visualize grid
scbi_cv_grid$plots +
  geom_sf(data = scbi_growth_df, aes(col=factor(foldID)), size = 0.1)
```

### Model specification

Next we specify the growth model we want. We will model growth as a
function of tree identity, size (in dbh), and the identity and size (in
dbh) of its neighbors. The model can be run on different grouping of
individuals (e.g., based on species or trait groupings). It can also be
run with different notions of competition:

  - `model_number = 1`: No competition growth only depends on focal tree
    `dbh` and grouping.
  - `model_number = 2`: Competition but identity of competitor does not
    matter, just sum of competitior basal area.
  - `model_number = 3`: Full model with competition including competitor
    identity.

**Big Woods**:

``` r
bw_specs <- bw_growth_df %>% 
  get_model_specs(model_number = 3, species_notion = 'trait_group')
bw_specs
#> $model_formula
#> growth ~ trait_group + dbh + dbh * trait_group + comp_basal_area + 
#>     comp_basal_area * trait_group + evergreen * trait_group + 
#>     maple * trait_group + misc * trait_group + oak * trait_group + 
#>     short_tree * trait_group + shrub * trait_group
#> <environment: 0x7fa73ada7320>
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
```

Next we create the `focal_vs_comp` data frame which connects each tree
in `growth_df` to the trees in its competitive neighborhood range (as
defined by `max_dist`). So for example, if `growth_df` consisted of two
focal trees with two and three neighbors respectively, `focal_vs_comp`
would consist of 5 rows.

This requires the `growth_df` data frame, `max_dist` scalar defining
competitive range, `cv_fold_size` defining the size of the spatial
cross-validation blocks, and `model_specs` as inputs.

``` r
if (!file.exists("focal_vs_comp_bw.Rdata")) {
  tic()
  focal_vs_comp_bw <- bw_growth_df %>% 
    create_focal_vs_comp(max_dist, model_specs = bw_specs, cv_grid = bw_cv_grid, id = "treeID")
  toc()
} else {
  load("focal_vs_comp_bw.Rdata")
}
#> 205.044 sec elapsed
```

``` r
glimpse(focal_vs_comp_bw)
#> Rows: 431,244
#> Columns: 10
#> $ focal_ID                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ focal_notion_of_species <fct> oak, oak, oak, oak, oak, oak, oak, oak, oak, …
#> $ dbh                     <dbl> 41.2, 41.2, 41.2, 41.2, 41.2, 41.2, 41.2, 41.…
#> $ foldID                  <int> 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 1…
#> $ geometry                <POINT> POINT (8.7 107.5), POINT (8.7 107.5), POINT…
#> $ growth                  <dbl> 0.40377706, 0.40377706, 0.40377706, 0.4037770…
#> $ comp_ID                 <dbl> 2, 3, 4, 5, 6, 7, 8, 81, 82, 83, 84, 85, 86, …
#> $ dist                    <dbl> 2.370654, 3.413210, 3.905125, 4.162932, 4.601…
#> $ comp_notion_of_species  <fct> evergreen, maple, maple, maple, maple, maple,…
#> $ comp_basal_area         <dbl> 0.0027339710, 0.1604599864, 0.0013202543, 0.0…
```

**SCBI**:

``` r
scbi_specs <- scbi_growth_df %>% 
  get_model_specs(model_number = 3, species_notion = 'sp')
scbi_specs
```

``` r
tic()
focal_vs_comp_scbi <- scbi_growth_df %>% 
  create_focal_vs_comp(max_dist, model_specs = scbi_specs, cv_grid = scbi_cv_grid, id = "stemID")
toc()
```

``` r
glimpse(focal_vs_comp_scbi)
```

### Model fit and prediction

**Big Woods**:

Now we are ready to fit the competition model with `fit_bayesian_model`
this takes two inputs: `focal_vs_comp` the data frame with focal trees
connected to their competitors and `model_specs` which specifies the
notion of competition.

``` r
tic()
bw_fit_model <- focal_vs_comp_bw %>% 
  fit_bayesian_model(model_specs = bw_specs)
toc()
#> 2.164 sec elapsed
```

This output has the posterior parameters for the specified competition
model. If `model_number = 1` this includes just the beta values for how
focal tree DBH and identity affect growth. If `model_number = 2` it also
includes a vector of lambdas for how total competitor basal area affect
individuals of each group. If `model_number = 3` then lambda is a matrix
which gives how individuals of each focal group respond to the
competition of each competitor group. This `posterior_param` output can
be used to get predicted growths for each individual (with
`predict_bayesain_model`) to test how well the model performs. Or this
`posterior_param` output can be plots (either the betas or lambdas) to
understand what controls individual growth.

``` r
bw_growth_df <- focal_vs_comp_bw %>% 
  predict_bayesian_model(model_specs = bw_specs, posterior_param = bw_fit_model) %>% 
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
  
bw_growth_df <- bw_growth_df %>% 
  st_as_sf()

# Observed vs predicted growth  
ggplot(bw_growth_df, aes(x = growth, y = growth_hat)) +
  geom_point(size = 0.5, color = rgb(0, 0, 0, 0.25)) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() + 
  labs(
    x = "Observed growth in dbh", y = "Predicted growth in dbh", 
    title = "Predicted vs Observed Growth"
  )
```

<img src="man/figures/README-unnamed-chunk-22-1.png" width="100%" />

``` r

reslab <- expression(paste('Residual (cm ',y^{-1},')'))
bw_growth_df %>% 
  # Need to investigate missingness
  filter(!is.na(growth_hat)) %>% 
  mutate(
    error = growth - growth_hat,
    error_bin = cut_number(error, n = 5), 
    error_compress = ifelse(error < -0.75, -0.75, ifelse(error > 0.75, 0.75, error))
  ) %>% 
  ggplot() + 
  geom_sf(aes(col = error_compress), size = 0.4) + 
  theme_bw() + 
  scale_color_gradient2(
    low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf", 
    name = reslab,
    breaks = seq(from = -0.75, to = 0.75, by = 0.25),
    labels = c('< -0.75', '-0.5', '0.25', '0', '0.25', '0.5', '> 0.75')) +
  labs(x = "Meter", y = "Meter")
```

<img src="man/figures/README-unnamed-chunk-22-2.png" width="100%" />

**SCBI**:

``` r
tic()
scbi_fit_model <- focal_vs_comp_scbi %>% 
  fit_bayesian_model(model_specs = scbi_specs)
toc()
```

``` r
scbi_growth_df <- focal_vs_comp_scbi %>% 
  predict_bayesian_model(model_specs = scbi_specs, posterior_param = scbi_fit_model) %>% 
  right_join(scbi_growth_df, by = c("focal_ID" = "stemID"))

scbi_growth_df <- scbi_growth_df %>% 
  st_as_sf()

# Observed vs predicted growth  
ggplot(scbi_growth_df, aes(x = growth, y = growth_hat)) +
  geom_point(size = 0.5, color = rgb(0, 0, 0, 0.25)) +
  stat_smooth(method = 'lm') +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() + 
  labs(
    x = "Observed growth in dbh", y = "Predicted growth in dbh", 
    title = "Predicted vs Observed Growth"
  )

reslab <- expression(paste('Residual (cm ',y^{-1},')'))
scbi_growth_df %>% 
  # Need to investigate missingness
  filter(!is.na(growth_hat)) %>% 
  mutate(
    error = growth - growth_hat,
    error_bin = cut_number(error, n = 5), 
    error_compress = ifelse(error < -0.75, -0.75, ifelse(error > 0.75, 0.75, error))
  ) %>% 
  ggplot() + 
  geom_sf(aes(col = error_compress), size = 0.4) + 
  theme_bw() + 
  scale_color_gradient2(
    low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf", 
    name = reslab,
    breaks = seq(from = -0.75, to = 0.75, by = 0.25),
    labels = c('< -0.75', '-0.5', '0.25', '0', '0.25', '0.5', '> 0.75')) +
  labs(x = "Meter", y = "Meter")
```

### Run spatial cross-validation

**SCBI**

For the above results we fit the model to the entire data set, and then
make predictions across the entire data set from that fit. This could
lead to overfitting because we are using the training data to also test
the model. If model error is spatially correlated this could be a large
issue (cite important sources here\!). We can use the spatial block
structure we defined above to deal with with. The function `run_cv` goes
through each fold in the `cv_grid` and fits the model on all the other
folds. Then applies that fit to the focal fold. It is a wrapper for
`fit_bayesain_model` and `predict_bayesain_model` but fits a seperate
model for each fold.

This will fit the model for each fold. On each fold it fits the data for
all trees outside of that fold. If you have N folds then `run_cv` will
take N-times longer than `fit_bayesain_model`. Here I cheated `run_cv`
to fit for just `test = fold 23` and `test = fold 2`. It still fits to
all other folds for those two, but only does it twice (rather than 28
times for the SCBI data set). Do this with the arguement `all_folds =
FALSE`.

``` r
tic()
scbi_cv_predict <- focal_vs_comp_scbi %>%
  run_cv(model_specs = scbi_specs, max_dist = max_dist, cv_grid = scbi_cv_grid, all_folds = FALSE)
toc()
```

Running just two folds took 2119 seconds. There are 28 folds. So running
everything should take 2119 \* 14 / 60 / 60 = 8 hours.

Then we can compare the results to show that the RMSE for the
cross-validated fit is larger than for the none CV fit above.

``` r
scbi_cv_predict %>%
  inner_join(scbi_growth_df, by = 'focal_ID', suffix = c('_cv','')) %>%
  summarise(
    rmse_cv = sqrt(mean((growth - growth_hat_cv)^2)),
    rmse = sqrt(mean((growth - growth_hat)^2)), 
    n = n() 
  )
```

**Big Woods**

Big woods is faster because it has fewer trees, but also because we are
fitting the model for trait groups (6 groups) rather than species for
SCBI (40 species). The lambda matrix is much smaller (6 x 6 versus 40 x
40). Means it fits much faster.

``` r
tic()
bw_cv_predict <- focal_vs_comp_bw %>%
  run_cv(model_specs = bw_specs, max_dist = max_dist, cv_grid = bw_cv_grid)
toc()
#> 278.514 sec elapsed
```

Big Woods must faster because it is a smaller plot? Mabye also because
we are fitting the trait group version (6 spp versus 40 for SCBI).

We have a problem: not all focal trees have a CV predicted value of
growth\_hat

``` r
bw_growth_df %>% 
  left_join(bw_cv_predict, by = 'focal_ID', suffix = c('', '_cv')) %>% 
  mutate(has_cv = !is.na(growth_hat_cv)) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(size = 0.1, aes(col = has_cv))
```

<img src="man/figures/README-unnamed-chunk-28-1.png" width="100%" />

``` r
bw_growth_df %>% 
  left_join(bw_cv_predict, by = 'focal_ID', suffix = c('', '_cv')) %>%
  filter(!is.na(growth_hat_cv)) %>% 
  as_tibble() %>% 
  summarise(
    rmse_cv = sqrt(mean((growth - growth_hat_cv)^2)),
    rmse = sqrt(mean((growth - growth_hat)^2)), 
    n = n() 
  )
#> # A tibble: 1 x 3
#>   rmse_cv  rmse     n
#>     <dbl> <dbl> <int>
#> 1   0.161 0.160 15848
```

### Run permutations

  - Permutation test:
      - Add line to that shuffles competitor species within
        `focal_and_comp` just before fitting model
      - permutation AND CV: add `run_shuffle` argument to `run_cv()`

### Visualize posterior distributions

  - Plot posterior distributions of all parameters
