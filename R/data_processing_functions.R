#' Compute growth of trees
#'
#' Based on two tree censuses, compute the average annual growth in `dbh` for all
#' trees.
#'
#' @param census_1 A data frame of the first census.
#' @param census_2 A data frame of the second (later) census
#' @param id Name of variable that uniquely identifies each tree common
#' to \code{census_1} and \code{census_2} allowing you to join/merge
#' both data frames.
#'
#' @return An `sf` data frame with column \code{growth} giving the average
#' annual growth in `dbh`.
#'
#' @import dplyr
#' @import sf
#'
#' @family data processing functions
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#'
#' growth_ex <-
#'   compute_growth(
#'     census_1 = census_1_ex %>%
#'       mutate(sp = to_any_case(sp) %>% factor()),
#'     census_2 = census_2_ex %>%
#'       filter(!str_detect(codes, "R")) %>%
#'       mutate(sp = to_any_case(sp) %>% factor()),
#'     id = "ID"
#'   )
compute_growth <- function(census_1, census_2, id) {
  # 0. Check inputs
  check_inherits(census_1, "data.frame")
  check_inherits(census_2, "data.frame")
  check_inherits(id, "character")

  purrr::map2(
    c(id, "gx", "gy", "date", "codes", "dbh"),
    c("numeric", "numeric", "numeric", "Date", "character", "numeric"),
    check_column,
    census_1
  )
  purrr::map2(
    c(id, "gx", "gy", "date", "codes", "dbh"),
    c("numeric", "numeric", "numeric", "Date", "character", "numeric"),
    check_column,
    census_2
  )

  if (
    (!id %in% colnames(census_1)) | (n_distinct(census_1[[id]]) != nrow(census_1)) |
    (!id %in% colnames(census_2)) | (n_distinct(census_2[[id]]) != nrow(census_2))
  ){
    glue_stop("The `id` argument must be the name of a numeric variable in both `census_1` and `census_2` that uniquely identifies each row.")
  }


  # Limit second census data to only those variables that can change
  census_2 <- census_2 %>%
    select(all_of(id), dbh, date, codes)

  growth_df <- census_1 %>%
    filter(dbh > 0) %>%
    # TODO: Hey Dave, don't we want inner_join here then?
    # left_join because we want all trees from census 1 (competitors) but
    # only want trees from census 2 that were alive in 1 (to see how much they grew)
    left_join(census_2, by = id, suffix = c("1", "2")) %>%
    # Compute avg annual growth:
    mutate(
      n_days = difftime(date2, date1),
      n_days = as.numeric(n_days),
      n_years = n_days / 365.25,
      growth = (dbh2 - dbh1) / n_years,
      # Convert species to factor:
      sp = factor(sp)
    ) %>%
    select(-c(n_days, n_years, date1, date2)) %>%
    st_as_sf(coords = c("gx", "gy"))

  return(growth_df)
}





#' Create focal versus competitor trees data frame
#'
#' @param growth_df A [compute_growth()] output converted to \code{sf} object
#' @param blocks An sf object of a \code{blockCV} block output
#' @param comp_dist Distance to determine which neighboring trees to a focal tree are competitors.
#' @param id A character string of the variable name in \code{growth_df} uniquely identifying each tree
#'
#' @return \code{focal_vs_comp} data frame of all focal trees and for each focal
#'   tree all possible competitor trees. In particular, for each competitor tree
#'   we compute the \href{https://en.wikipedia.org/wiki/Basal_area}{basal area}
#'   (in meters-squared) based on the `dbh1` variable from the first census
#'   (assumed to be in cm).
#'
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom purrr map2
#'
#' @description "Focal versus competitor trees" data frames are the main data
#'   frame used for analysis. "Focal trees" are all trees that satisfy the
#'   following criteria
#' \tabular{l}{
#'   1. Were alive at both censuses \cr
#'   2. Were not part of the study region's buffer as computed by [add_buffer_variable()] \cr
#'   3. Were not a resprout at the second census. Such trees should be coded as
#'   `"R"` in the `codes2` variable (OK if a resprout at first census)
#' }
#' For each focal tree, "competitor trees" are all trees that (1) were alive at
#' the first census and (2) within \code{comp_dist} distance of the focal tree.
#'
#' @note In order to speed computation, in particular of distances between all
#'   focal/competitor tree pairs, we use the cross-validation \code{blockCV}
#'   object to divide the study region into smaller subsets.
#'
#' @family data processing functions
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#' library(sf)
#' library(sfheaders)
#' library(tibble)
#'
#' # Create fold information sf object.
#' SpatialBlock_ex <-
#'   tibble(
#'     # Study region boundary
#'     x = c(0, 0, 5, 5),
#'     y = c(0, 5, 5, 0)
#'   ) %>%
#'   # Convert to sf object
#'   sf_polygon() %>%
#'   mutate(folds = "1")
#'
#' # Plot example data. Observe for comp_dist = 1.5, there are 6 focal vs comp pairs:
#' # 1. focal 1 vs comp 2
#' # 2. focal 2 vs comp 1
#' # 3. focal 2 vs comp 3
#' # 4. focal 3 vs comp 2
#' # 5. focal 4 vs comp 5
#' # 6. focal 5 vs comp 4
#' ggplot() +
#'   geom_sf(data = SpatialBlock_ex, fill = "transparent") +
#'   geom_sf_label(data = growth_toy, aes(label = ID))
#'
#' # Return corresponding data frame
#' growth_toy %>%
#'   create_focal_vs_comp(comp_dist = 1.5, blocks = SpatialBlock_ex, id = "ID")
#'
#' # Load in growth_df with spatial data
#' # See ?growth_ex for attaching spatial data to growth_df
#' data(growth_spatial_ex)
#' # Load in blocks
#' data(blocks_ex)
#'
#' focal_vs_comp_ex <- growth_spatial_ex %>%
#'   create_focal_vs_comp(comp_dist = 1, blocks = blocks_ex, id = "ID")
create_focal_vs_comp <- function(growth_df, comp_dist, blocks, id) {
  # 0. Check inputs
  check_inherits(growth_df, "data.frame")
  check_inherits(comp_dist, "numeric")
  check_inherits(blocks, "sf")
  check_inherits(id, "character")

  if (!id %in% colnames(growth_df)) {
    glue_stop("The `id` argument must be the name of a column in both `census_1` and `census_2` that uniquely identifies each row.")
  }

  purrr::map2(
    c(id, "sp", "dbh1", "dbh2", "growth", "geometry", "buffer", "foldID"),
    c("numeric", "factor", "numeric", "numeric", "numeric", "sfc", "logical", "factor"),
    check_column,
    growth_df
  )

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
  # within comp_dist distance of it, and save as data frame. Note that to we do
  # this fold-by-fold using the previously computed blockCV grid object. We do
  # this to acceleration computation, in particular all distance pairs.
  all_folds <- focal_trees$foldID %>%
    unique()
  focal_vs_comp <- vector(mode = "list", length = length(all_folds))

  for (i in 1:length(all_folds)) {
    # Identify this fold's boundary
    fold_boundary <- blocks %>%
      filter(folds == all_folds[i])

    # Identify focal trees in this fold
    focal_trees_fold <- focal_trees %>%
      filter(foldID == all_folds[i])

    # Identify comp trees in this fold: both trees inside fold and those within
    # comp_dist distance outwards of fold boundary
    comp_trees_fold <- comp_trees %>%
      add_buffer_variable(direction = "out", size = comp_dist, region = fold_boundary) %>%
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
        # geom_sf(data = study_region_bw, col = "black", fill = "transparent", linetype = "dashed") +
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
      # Remove pairs more than comp_dist apart
      filter(dist < comp_dist) %>%
      # join focal tree data
      left_join(focal_trees_fold, by = "focal_ID") %>%
      # Join competitor tree data
      left_join(comp_trees_fold, by = "comp_ID") %>%
      # Clean up mess from join:
      select(-c(foldID.y, geometry.y)) %>%
      rename(foldID = foldID.x, geometry = geometry.x)
  }


  # 4. Return output data frame
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
    ) %>%
    nest(comp = c(comp_ID, dist, comp_sp, comp_basal_area))

  return(focal_vs_comp)
}
