#' Compute growth of trees
#'
#' Based on two tree censuses compute the average annual growth in dbh for all
#' trees that were alive at (TODO: fill this in).
#'
#' @param census_df1 A data frame of the first census.
#' @param census_df2 A data frame of the second (later) census
#' @param id Name of variable common to \code{census_df1} and \code{census_df2}
#' allowing you to join/merge both data frames.
#'
#' @return growth_df An sf data frame with \code{growth}: average annual growth in dbh.
#' @export
#' @import dplyr
#' @import sf
#' @examples
#' library(dplyr)
#' library(stringr)
#' # Load in data from two forst censuses
#' data(census_df1_ex, census_df2_ex)
#' # Filter out resprouts in second census
#' census_df2_ex_no_r <- census_df2_ex %>%
#'   filter(!str_detect(codes, "R"))
#' id <- "ID"
#' ex_growth_df <- compute_growth(census_df1_ex, census_df2_ex_no_r, id)
compute_growth <- function(census_df1, census_df2, id) {

  # TODO: Write following checks
  # - Both census data frames have variables: id, dbh, date, and codes.
  # - Check variable types: chr, dbl, date/dttm, NA
  # - Check that id uniquely identifies rows
  # - Prompt use with message: "Assuming dbh are in cm"

  # Limit second census data to only those variables that can change
  census_df2 <- census_df2 %>%
    select(all_of(id), dbh, date, codes)

  growth_df <- census_df1 %>%
    filter(dbh > 0) %>%
    # TODO: Hey Dave, don't we want inner_join here then?
    # left_join because we want all trees from census 1 (competitors) but
    # only want trees from census 2 that were alive in 1 (to see how much they grew)
    left_join(census_df2, by = id, suffix = c("1", "2")) %>%
    # Compute avg annual growth:
    mutate(
      n_days = difftime(date2, date1),
      n_days = as.numeric(n_days),
      n_years = n_days / 365.25,
      growth = (dbh2 - dbh1) / n_years
    ) %>%
    select(-c(n_days, n_years, date1, date2)) %>%
    st_as_sf(coords = c("gx", "gy"))

  return(growth_df)
}





#' Create focal versus competitor trees data frame
#'
#' @param growth_df A \code{\link{compute_growth}} output converted to \code{sf} object
#' @param cv_grid_sf An sf object of a \code{blockCV} block output
#' @param max_dist Distance to determine which neighboring trees to a focal tree are competitors.
#' @param id A character string of the variable name in \code{growth_df} uniquely identifying each tree
#' @return \code{focal_vs_comp} data frame of all focal trees and for each focal
#'   tree all possible competitor trees. In particular, for each competitor tree
#'   we compute the \href{https://en.wikipedia.org/wiki/Basal_area}{basal area}
#'   (in meters-squared) based on the `dbh1` variable from the first census
#'   (assumed to be in cm).
#' @export
#' @import dplyr
#' @description "Focal versus competitor trees" data frames are the main data
#'   frame used for analysis. "Focal trees" are all trees that satisfy the
#'   following criteria
#' \tabular{l}{
#'   1. Were alive at both censuses \cr
#'   2. Were not part of the study region's buffer as computed by \code{\link{add_buffer_variable}} \cr
#'   3. Were not a resprout at the second census. Such trees should be coded as
#'   `"R"` in the `codes2` variable (OK if a resprout at first census)
#' }
#' For each focal tree, "competitor trees" are all trees that (1) were alive at
#' the first census and (2) within \code{max_dist} distance of the focal tree.
#' @note In order to speed computation, in particular of distances between all
#'   focal/competitor tree pairs, we use the cross-validation \code{blockCV}
#'   object to divide the study region into smaller subsets.
#' @seealso \code{\link{focal_vs_comp_distance}}
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#' library(sf)
#' library(sfheaders)
#' library(tibble)
#'
#' # Create fold information sf object. TODO: clean this
#' cv_grid_ex <-
#'   tibble(
#'     # Study region boundary
#'     x = c(0, 0, 5, 5),
#'     y = c(0, 5, 5, 0)
#'   ) %>%
#'   # Convert to sf object
#'   sf_polygon() %>%
#'   mutate(folds = "1")
#'
#' # Plot example data. Observe for max_dist = 1.5, there are 6 focal vs comp pairs:
#' # 1. focal 1 vs comp 2
#' # 2. focal 2 vs comp 1
#' # 3. focal 2 vs comp 3
#' # 4. focal 3 vs comp 2
#' # 5. focal 4 vs comp 5
#' # 6. focal 5 vs comp 4
#' ggplot() +
#'   geom_sf(data = cv_grid_ex, fill = "transparent") +
#'   geom_sf_label(data = growth_df_ex, aes(label = ID))
#'
#' # Return corresponding data frame
#' growth_df_ex %>%
#'   create_focal_vs_comp(max_dist = 1.5, cv_grid_sf = cv_grid_ex, id = "ID")
#'
#' # Load in growth_df with spatial data
#' # See ?ex_growth_df for attaching spatial data to growth_df
#' data(ex_growth_df_spatial)
#' # Load in cv_grid
#' data(ex_cv_grid_sf)
#'
#' focal_vs_comp_ex <- ex_growth_df_spatial %>%
#'   create_focal_vs_comp(max_dist = 1, cv_grid_sf = ex_cv_grid_sf, id = "ID")
create_focal_vs_comp <- function(growth_df, max_dist, cv_grid_sf, id) {
  # TODO: Create example for this function using toy dataset
  # TODO: Inputs checks that growth_df has sp variable, maybe id variable

  # 1. Define focal trees
  focal_trees <- growth_df %>%
    # Identify trees that satisfy focal tree criteria
    filter(dbh1 > 0, dbh2 > 0, !buffer, codes2 != "R") %>%
    # Define notion of species as factor
    rename(dbh = dbh1) %>%
    # ID numbers to join focal trees with competitor trees
    mutate(focal_ID = .data[[id]]) %>%
    select(focal_ID, foldID, geometry, growth, focal_sp = sp, dbh)


  # 2. Define set of candidate competitor trees:
  comp_trees <- growth_df %>%
    # Identify trees that satisfy competitor tree criteria
    filter(dbh1 > 0) %>%
    mutate(
      comp_ID = .data[[id]],
      # Compute basal area using dbh1 from first census
      comp_basal_area = 0.0001 * pi * (dbh1 / 2)^2
    ) %>%
    select(comp_ID, foldID, comp_sp = sp, comp_basal_area)


  # 3. For each focal tree, identify all candidate competitor trees that are
  # within max_dist distance of it, and save as data frame. Note that to we do
  # this fold-by-fold using the previously computed blockCV grid object. We do
  # this to acceleration computation, in particular all distance pairs.
  all_folds <- focal_trees$foldID %>%
    unique()
  focal_vs_comp <- vector(mode = "list", length = length(all_folds))

  for (i in 1:length(all_folds)) {
    # Identify this fold's boundary
    fold_boundary <- cv_grid_sf %>%
      filter(folds == all_folds[i])

    # Identify focal trees in this fold
    focal_trees_fold <- focal_trees %>%
      filter(foldID == all_folds[i])

    # Identify comp trees in this fold: both trees inside fold and those within
    # max_dist distance outwards of fold boundary
    comp_trees_fold <- comp_trees %>%
      add_buffer_variable(direction = "out", size = max_dist, region = fold_boundary) %>%
      filter(!buffer)

    if (FALSE) {
      # Sanity check plot: for the ith fold, smaller black dots are competitor
      # trees and cyan larger dots are the test set. orange ones separating test
      # set from training set (trees in all other folds)
      ggplot() +
        # Focal & competitor trees:
        geom_sf(data = comp_trees_fold, col = "orange", size = 3, ) +
        geom_sf(data = focal_trees_fold, col = "black", size = 1, fill = "transparent") +
        # Boundaries
        # geom_sf(data = bw_study_region, col = "black", fill = "transparent", linetype = "dashed") +
        geom_sf(data = fold_boundary, col = "black", fill = "transparent") +
        labs(
          title = str_c("Fold ", all_folds[i], ": Focal = black, competitor = orange")
        )
    }

    # Save current fold info
    focal_vs_comp[[i]] <-
      # Take focal trees in this fold
      focal_trees_fold %>%
      # Compute distances to all competitor trees in this fold
      focal_vs_comp_distance(comp_trees_fold) %>%
      # Remove pairs more than max_dist apart
      filter(dist < max_dist) %>%
      # join focal tree data
      left_join(focal_trees_fold, by = "focal_ID") %>%
      # Join competitor tree data
      left_join(comp_trees_fold, by = "comp_ID") %>%
      # Clean up mess from join:
      select(-c(foldID.y, geometry.y)) %>%
      rename(foldID = foldID.x, geometry = geometry.x)
  }


  # 4. Return output data frame
  # TODO: Questions to consider
  # 1. Should we make this a nested-list object?
  # 2. Should we convert to sf object using st_as_sf() here?
  focal_vs_comp <- focal_vs_comp %>%
    # Convert list to tibble:
    bind_rows() %>%
    arrange(focal_ID, comp_ID) %>%
    mutate(growth_hat = NA) %>%
    select(
      # Relating to focal tree:
      focal_ID, focal_sp, dbh, foldID, geometry, growth,
      # Relating to competitor tree:
      comp_ID, dist, comp_sp, comp_basal_area
    )

  return(focal_vs_comp)
}