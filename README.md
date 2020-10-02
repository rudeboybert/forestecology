
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
library(yardstick)
library(viridis)
library(lubridate)

run_scbi <- FALSE
run_bw <- TRUE
```

### Load & preprocess data

The data for the Big Woods plor are included in the package. For SCBI,
we load and preprocess the data corresponding to the two census
reflecting the time period over which we consider growth in dbh. We load
them into R as “tibble” data frames thereby ensuring a standardized
input/output format that can be used across all `tidyverse` packages
@tidyverse. Furthermore, we ensure that the different variables have the
correct names, types (`dbl`, `data`, `factor`).

**Big Woods**:

``` r
# Big Woods
# Read in census data from 2008 & 2014
data(bw_census_2008, bw_census_2014, bw_species)

# Append additional species data
bw_census_2008 <- bw_census_2008 %>%
  left_join(bw_species, by = "sp") %>%
  select(-c(genus, species, latin))
```

**SCBI**:

``` r
# SCBI
scbi_2013 <- 
  "https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/census-csv-files/scbi.stem2.csv" %>% 
  read_csv() %>% 
  select(
    treeID, stemID, sp, quadrat, gx, gy, dbh, 
    date = ExactDate, codes, status
  ) %>%
  mutate(date = mdy(date))

scbi_2018 <-
  "https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/census-csv-files/scbi.stem3.csv" %>% 
  read_csv() %>% 
  select(
    treeID, stemID, sp, quadrat, gx, gy, dbh, 
    date = ExactDate, codes, status
  ) %>% 
  mutate(dbh = as.numeric(dbh),
         date = mdy(date)) 
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
# Big Woods
id <- "treeID"

# we need to filter out the resprouts
bw_census_2014 <- bw_census_2014 %>% 
  filter(!str_detect(codes, 'R'))

bw_growth_df <-
  # Merge both censuses and compute growth:
  compute_growth(bw_census_2008, bw_census_2014, id) %>%
  mutate(
    sp = to_any_case(sp),
    sp = as.factor(sp),
    species = sp,
    family = as.factor(family),
    trait_group = as.factor(trait_group)) %>%
  # drop stemID
  select(-stemID)
```

**SCBI**:

``` r
# SCBI
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
# Both Big Woods & SCBI
growth_df <- bind_rows(
  bw_growth_df %>% st_drop_geometry %>% select(growth) %>% mutate(site = "bw"),
  scbi_growth_df %>% st_drop_geometry %>% select(growth) %>% mutate(site = "scbi")
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

We use the function `add_buffer_variable` to identifiy which trees are
inside of a buffer region of size `max_dist`. The user will need to
specify a study region and convert to a `sf_polygon`.

**Example of buffer at work**:

``` r
# Boundary polygon
square_boundary <- tibble(
  x = c(0,0,1,1),
  y = c(0,1,1,0)
) %>% 
  sf_polygon()

# "Trees" in polygon
trees_df <- tibble(
    x = runif(100),
    y = runif(100)
  ) %>%
  sf_point()

# Buffer polygon
trees_df <- trees_df %>%
  add_buffer_variable(direction = "in", size = 0.1, region = square_boundary)

ggplot() +
  geom_sf(data = trees_df, aes(col = buffer))
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

**Big Woods**:

``` r
# Bigwoods
data(bw_study_region)

# Add buffer variable to data frame
bw_growth_df <- bw_growth_df %>%
  add_buffer_variable(direction = "in", size = max_dist, region = bw_study_region)

ggplot() +
  geom_sf(data = bw_growth_df, aes(col = buffer))
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

**SCBI**:

``` r
# SCBI
# Study region boundary polygon
scbi_study_region <- tibble(x = c(0,400,400,0,0), y = c(0,0,640,640,0)) %>% 
  sf_polygon()

# Add buffer variable to data frame
scbi_growth_df <- scbi_growth_df %>%
  add_buffer_variable(direction = "in", size = max_dist, region = scbi_study_region)

ggplot() +
  geom_sf(data = scbi_growth_df, aes(col = buffer))
```

#### Defining spatial cross-validation folds

We use the [`blockCV`](https://github.com/rvalavi/blockCV) package to
define the spatial grid, whose elements will act as the folds in our
leave-one-out (by “one” we meen “one grid block”) cross-validation
scheme.

**Big Woods**:

``` r
# Big Woods
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

bw_growth_df <- bw_growth_df %>%
  filter(!foldID %in% c(19, 23, 21, 17, 8, 19)) %>%
  mutate(foldID = as.character(foldID))

# Deliverable
ggplot() +
  geom_sf(data = bw_growth_df, aes(col = foldID, alpha = buffer))
```

<img src="man/figures/README-unnamed-chunk-13-3.png" width="100%" />

``` r

bw_cv_grid_sf <- bw_cv_grid$blocks %>%
  st_as_sf()
```

**SCBI**:

``` r
# SCBI
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

scbi_cv_grid_sf <- scbi_cv_grid$blocks %>%
  st_as_sf()
```

### Focal versus competitor trees

Next we create the `focal_vs_comp` data frame which connects each tree
in `growth_df` to the trees in its competitive neighborhood range (as
defined by `max_dist`). So for example, if `growth_df` consisted of two
focal trees with two and three neighbors respectively, `focal_vs_comp`
would consist of 5 rows.

This requires the `growth_df` data frame; `max_dist`, the scalar
defining competitive range; `cv_fold_size`, defining the size of the
spatial cross-validation blocks; and the `id` variable as inputs.

``` r
# Big Woods
if (!file.exists("focal_vs_comp_bw.Rdata")) {
  tic()
  focal_vs_comp_bw <- bw_growth_df %>% 
    create_focal_vs_comp(max_dist, cv_grid_sf = bw_cv_grid_sf, id = "treeID")
  toc()
  save(focal_vs_comp_bw, file = "focal_vs_comp_bw.Rdata")
} else {
  load("focal_vs_comp_bw.Rdata")
}
```

``` r
# Big Woods
glimpse(focal_vs_comp_bw)
#> Rows: 454,718
#> Columns: 10
#> $ focal_ID        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ focal_sp        <fct> white_oak, white_oak, white_oak, white_oak, white_oak…
#> $ dbh             <dbl> 41.2, 41.2, 41.2, 41.2, 41.2, 41.2, 41.2, 41.2, 41.2,…
#> $ foldID          <chr> "12", "12", "12", "12", "12", "12", "12", "12", "12",…
#> $ geometry        <POINT> POINT (8.7 107.5), POINT (8.7 107.5), POINT (8.7 10…
#> $ growth          <dbl> 0.40377706, 0.40377706, 0.40377706, 0.40377706, 0.403…
#> $ comp_ID         <dbl> 2, 3, 4, 5, 6, 7, 8, 81, 82, 83, 84, 85, 86, 87, 213,…
#> $ dist            <dbl> 2.370654, 3.413210, 3.905125, 4.162932, 4.601087, 6.3…
#> $ comp_sp         <fct> serviceberry, black_cherry, red_maple, red_maple, red…
#> $ comp_basal_area <dbl> 0.0027339710, 0.1604599864, 0.0013202543, 0.001661902…
```

**SCBI**:

``` r
# SCBI
tic()
focal_vs_comp_scbi <- scbi_growth_df %>% 
  create_focal_vs_comp(max_dist, cv_grid_sf = scbi_cv_grid_sf, id = "stemID")
toc()
```

``` r
# SCBI
glimpse(focal_vs_comp_scbi)
```

### Model fit and prediction

**Big Woods**:

Now we are ready to fit the competition model with `fit_bayesian_model`.
This function needs only the `focal_vs_comp` as an input. Other options
allow the user to specify prior parameters and run a species identity
shuffle (see below).

``` r
# Big Woods
tic()
posterior_param_bw <- focal_vs_comp_bw %>% 
  fit_bayesian_model(prior_param = NULL, run_shuffle = FALSE)
toc()
#> 211.59 sec elapsed
```

This output has the posterior parameters for the specified competition
model. This `posterior_param` output can be used to get predicted
growths for each individual (with `predict_bayesain_model`) to test how
well the model performs. Or this `posterior_param` output can be plots
(either the betas or lambdas) to understand what controls individual
growth.

Here we calculate the RMSE

``` r
# Big Woods
predictions <- focal_vs_comp_bw %>%
  predict_bayesian_model(posterior_param = posterior_param_bw) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
predictions %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
#> [1] 0.148145
```

Now we test whether the identity of the competitor matters. We do this
by shuffling the identity of competitors (but not of focal trees or
spatial locations or sizes) and fitting the model again. We then compare
RMSEs to see whether competitor identity matters to competitive effects

``` r
posterior_param_bw_shuffle <- focal_vs_comp_bw %>%
  fit_bayesian_model(prior_param = NULL, run_shuffle = TRUE)

# b) Make predictions and compute RMSE
predictions_shuffle <- focal_vs_comp_bw %>%
  predict_bayesian_model(posterior_param = posterior_param_bw_shuffle) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
predictions_shuffle %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
#> [1] 0.1505954
```

The RMSE is lower for the non-shuffled version. This gives support for
the idea that competitor identiy does matter for competitive
interactions.

**SCBI**:

``` r
# SCBI
tic()
posterior_param_scbi <- focal_vs_comp_scbi %>% 
  fit_bayesian_model(prior_param = NULL, run_shuffle = TRUE)
toc()
```

``` r
# SCBI
scbi_growth_df <- focal_vs_comp_scbi %>% 
  predict_bayesian_model(posterior_param = posterior_param_scbi) %>% 
  right_join(scbi_growth_df, by = c("focal_ID" = "stemID"))

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
  st_as_sf() %>%
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

For the above results we fit the model to the entire data set, and then
make predictions across the entire data set from that fit. This could
lead to overfitting because we are using the training data to also test
the model. If model error is spatially correlated this could be a large
issue (cite important sources here\!). We can use the spatial block
structure we defined above to deal with with. The function `run_cv` goes
through each fold in the `cv_grid` and fits the model on all the other
folds. Then applies that fit to the focal fold. It is a wrapper for
`fit_bayesain_model` and `predict_bayesain_model` but fits a separate
model for each fold.

``` r
# big woods
tic()
cv_bw <- focal_vs_comp_bw %>%
  run_cv(max_dist = max_dist, cv_grid = bw_cv_grid) %>%
  right_join(bw_growth_df, by = c("focal_ID" = "treeID"))
toc()
#> 1663.303 sec elapsed

cv_bw %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
#> [1] 0.1533511
```

As expected this RMSE is higher than that when the model is fit without
cross validation. See Allen and Kim (2020) for more discussion of this.

``` r
# SCBI
tic()
cv_scbi <- focal_vs_comp_scbi %>%
  run_cv(max_dist = max_dist, cv_grid = scbi_cv_grid) %>%
  right_join(scbi_growth_df, by = c("focal_ID" = "treeID"))
toc()

cv_scbi %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
```

### Visualize posterior distributions

  - Plot posterior distributions of all parameters

We might be interested in the posterior distributions of parameters. The
betas tell us about how fast each species grows and how this depends on
DBH. The lambdas, which are often of more interest, are the
species-specific competition coefficents The full lambda matrix gives
competition strength between species. There is a rich literature how
this matrix (cite).

Because of the structure of the `bw_fit_model` object we cannot simply
draw these curves based on the posterior distribution. `bw_fit_model`
gives the parameteres *compared* to a baseline. This is often not of
interest. So to display these parameters as we care about them we have
to sample from the baseline distrubiton and from the comparison one to
get the posterior distribution of interest.

Here we re-run the Big Woods model but using the family as the group for
comparison. This makes the posterior distributions easier to follow.
Also, surprisingly, grouping by family performed just as well as
grouping by species (see Allen and Kim 2020). First we re-run
`create_focal_vs_comp` and `fit_bayesian_model` with the grouping
variable as family.

``` r

focal_vs_comp_bw <- bw_growth_df %>%
  # mutate(sp = trait_group) %>%
  mutate(sp = family) %>%
  create_focal_vs_comp(max_dist = max_dist, cv_grid_sf = bw_cv_grid_sf, id = "treeID")

# a) Fit model (compute posterior parameters) with no permutation shuffling
posterior_param_bw <- focal_vs_comp_bw %>%
  fit_bayesian_model(prior_param = NULL, run_shuffle = FALSE)
```

Now the output of `fit_bayesian_model` is passed to
`plot_posterior_parameters`.

``` r
# b) Recreate Fig5 from Allen (2020): Posterior distributions of selected lambdas
posterior_plots <- plot_posterior_parameters(
  posterior_param = posterior_param_bw,
  sp_to_plot = c("cornaceae", "fagaceae", "hamamelidaceae", "juglandaceae", "lauraceae", "rosaceae", "sapindaceae", "ulmaceae")
)
```

The output is a list with three plots stored. The element `beta_0` gives
the growth intercept, i.e., how fast an individual of each group grows
independent of DBH).

``` r
posterior_plots[["beta_0"]]
```

<img src="man/figures/README-unnamed-chunk-28-1.png" width="100%" />

Next `beta_dbh` gives the DBH-growth slope for each group.

``` r
posterior_plots[["beta_dbh"]]
```

<img src="man/figures/README-unnamed-chunk-29-1.png" width="100%" />

Finally `lambda` gives the competition coeffiencts.

``` r
posterior_plots[["lambda"]]
```

<img src="man/figures/README-unnamed-chunk-30-1.png" width="100%" />
