
<!-- README.md is generated from README.Rmd. Please edit that file -->

# forestecology

[![Build 
Status](https://github.com/rudeboybert/forestecology/workflows/R-CMD-check/badge.svg)](https://github.com/rudeboybert/forestecology/actions)
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

This package is designed to work for spatially mapped, repeat censused
forests plots. The package has commands to fit models of tree growth
based on neighborhood competition which can be used to estimate
species-specific competition coefficients. The models fits can then be
evaluated using a spatial cross-validation scheme to detect possible
overfitting. Additionally these models can test whether the species
identity of competitors matters using a permutation test-style shuffling
of competitor identity (under the null hypothesis) and subsequently
evaluating if model performance changes. See Allen and Kim (2020) [A
permutation test and spatial cross-validation approach to assess models
of interspecific competition between
trees](https://doi.org/10.1371/journal.pone.0229930) for a full
description.

## Example analysis

Here we provide an example with a small dataset.

``` r
library(tidyverse)
library(forestecology)
library(sf)
library(sfheaders)
library(blockCV)
library(yardstick)
library(snakecase)
```

First we combine two example census data files into a single `tibble`
and compute the growth of all surviving individuals.

``` r
# Read in census files
data(census_df1_ex, census_df2_ex)

# Filter out resprouts
census_df2_ex_no_r <- census_df2_ex %>%
  filter(!str_detect(codes, "R"))

# Name of variable in tibble that uniquely identifies each stem:
id <- "ID"

# Merge both censuses and compute growth:
ex_growth_df <-
  compute_growth(census_df1_ex, census_df2_ex_no_r, id) %>%
  mutate(
    sp = to_any_case(sp),
    # Must be a factor
    sp = as.factor(sp)
  )
```

All growth models in the package assume that two individuals compete if
they are less than a specified distance away. Here we set this distance
in `max_dist` and put all individuals within this distance of a plot
boundary in a buffer (since we do not have full information about their
competitors).

``` r
# Load study region:
data("ex_study_region")

# Set max dist
max_dist <- 1

# Add buffer
ex_growth_df <- ex_growth_df %>%
  add_buffer_variable(direction = "in", size = max_dist, region = ex_study_region)

ggplot() +
  geom_sf(data = ex_growth_df, aes(col = buffer), size = 2)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

We then set blocks/folds for the spatial cross-validation scheme. We
this both manually and using the
[`blockCV`](https://github.com/rvalavi/blockCV) package. Note that here
we define the geometric structure of the folds manually using
`sfheaders::sf_polygon()`:

``` r
fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))
blocks <- bind_rows(
  sf_polygon(fold1),
  sf_polygon(fold2)
) %>%
  mutate(foldID = c(1, 2))
```

Next we assign each point to folds using `blockCV::spatialBlock()`

``` r
ex_cv_grid <- spatialBlock(
  speciesData = ex_growth_df,
  verbose = FALSE,
  k = 2,
  selection = "systematic",
  blocks = blocks,
  showBlocks = FALSE
)

# Add foldID to data
ex_growth_df <- ex_growth_df %>%
  mutate(foldID = ex_cv_grid$foldID %>% as.factor())

# Deliverable
ggplot() +
  geom_sf(data = ex_growth_df, aes(col = buffer, shape = foldID), size = 2) +
  geom_sf(data = blocks, fill = "transparent")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r

# Convert to sf object
ex_cv_grid_sf <- ex_cv_grid$blocks %>%
  st_as_sf()
```

We then create a `focal_v_comp` tibble which has a row for each
competing pair of individuals (each pair of individuals within
`max_dist` of one another):

``` r
focal_vs_comp_ex <- ex_growth_df %>%
  create_focal_vs_comp(max_dist, cv_grid_sf = ex_cv_grid_sf, id = "ID")
```

With `focal_v_comp`, we can then run the growth model and get
predictions from it. Here we run the model without cross validation.
Posterior estimates can be plotted to give, for example, the lambda
matrix of competition coefficients.

``` r
# Fit model
posterior_param_ex <- focal_vs_comp_ex %>%
  fit_bayesian_model(prior_param = NULL, run_shuffle = FALSE)

# Get predicted dbh values values
predictions <- focal_vs_comp_ex %>%
  predict_bayesian_model(posterior_param = posterior_param_ex) %>%
  right_join(ex_growth_df, by = c("focal_ID" = "ID"))

# Compute RMSE of true vs predicted dbh values
predictions %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
#> [1] 0.1900981

# Plot posteriors
plot_ex <- posterior_param_ex %>%
  plot_bayesian_model_parameters()
plot_ex[["lambda"]]
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

Here we repeat the process but with cross-validation. Note the increase
in RMSE, reflecting the fact that our original estimate of model error
was overly optimistic.

``` r
# Fit model with cross-validation
ex_bw <- focal_vs_comp_ex %>%
  run_cv(max_dist = max_dist, cv_grid = ex_cv_grid_sf) %>%
  right_join(ex_growth_df, by = c("focal_ID" = "ID"))

# Compute RMSE of true vs predicted dbh values
ex_bw %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)
#> [1] 0.4068709
```

For fuller examples on actual datasets see \_\_\_.
