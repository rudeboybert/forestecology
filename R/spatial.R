globalVariables(c(
  "x_dir", "y_dir"
))

#' Computer buffer to a region.
#'
#' @param region region to be buffered
#' @param direction "in" for buffers that are contained within \code{region}, "out" for buffers that contain \code{region}.
#' @param max_dist size of buffer in same units as geometry in \code{region}
#' @return As \code{sf} polygon object corresponding to buffer
#' @importFrom sfheaders sf_polygon
#' @importFrom sf st_buffer
#' @seealso \code{\link{add_buffer_variable}}
#' @export
#' @examples
#' library(tibble)
#' library(sfheaders)
#' library(ggplot2)
#'
#' # Example square region to be buffered (as sf object)
#' region <- tibble(
#'   x = c(0, 0, 1, 1),
#'   y = c(0, 1, 1, 0)
#' ) %>%
#'   sf_polygon()
#'
#' # Size of buffer
#' size <- 0.05
#'
#' # Compute "inwards" buffer
#' inwards_buffer_region <- region %>%
#'   compute_buffer_region(direction = "in", size = size)
#'
#' # Compute "outwards" buffer
#' outwards_buffer_region <- region %>%
#'   compute_buffer_region(direction = "out", size = size)
#'
#' # Plot all three regions:
#' ggplot() +
#'   geom_sf(data = outwards_buffer_region, col = "blue", fill = "transparent") +
#'   geom_sf(data = region, fill = "transparent") +
#'   geom_sf(data = inwards_buffer_region, col = "orange", fill = "transparent") +
#'   labs(title = "Regions: original (black), inwards buffer (orange), and outwards buffer (blue)")
compute_buffer_region <- function(region, direction = "in", size){
  # - Q: Force user to specify region as sf instead of just tibble object?
  # - Run tests on direction and size

  if(direction == "in") {
    size = -1 * abs(size)
  } else if (direction == "out"){
    size = abs(size)
  }

  buffer_region <- region %>%
    # sf_polygon() %>%
    st_buffer(dist = size)

  return(buffer_region)
}


#' Identify trees in the buffer region
#'
#' @param growth_df \code{\link{compute_growth}} data frame
#' @inheritParams compute_buffer_region
#' @return The same \code{\link{growth_df}} data frame but with a new boolean
#'   variable \code{buffer} indicating if a tree is in the study region buffer
#'   area. This uses \code{\link{compute_buffer_region}} to define the boundary
#'   of the buffer region.
#' @import dplyr
#' @importFrom sf st_intersects
#' @seealso \code{\link{compute_buffer_region}}
#' @export
#' @examples
#' library(tibble)
#' library(sfheaders)
#' library(ggplot2)
#'
#' # Example square region to be buffered
#' region <- tibble(
#'   x = c(0, 0, 1, 1),
#'   y = c(0, 1, 1, 0)
#' ) %>%
#'   sf_polygon()
#'
#' # Example points
#' study_points <- tibble(
#'   x = runif(50),
#'   y = runif(50)
#' ) %>%
#'   sf_point()
#'
#' # Size of buffer
#' size <- 0.05
#'
#' # Identify whether points are within size of boundary
#' study_points <- study_points %>%
#'   add_buffer_variable(direction = "in", size = size, region = region)
#'
#' # Plot study points coded by whether they are within size of boundary
#' p <- ggplot() +
#'   geom_sf(data = region, fill = "transparent") +
#'   geom_sf(data = study_points, aes(col = buffer))
#' p
#'
#' # Additionally, show buffer boundary in red
#' buffer_boundary <- region %>%
#'   compute_buffer_region(direction = "in", size = size)
#' p +
#'   geom_sf(data = buffer_boundary, col = "red", fill = "transparent")
add_buffer_variable <- function(growth_df, direction, size, region){
  buffer_boundary <- region %>%
    compute_buffer_region(direction, size = size)

  # TODO: Address why we need to do this:
  # https://github.com/r-spatial/lwgeom/issues/6 based on googling error message
  # if you don't do this:
  # "OGR: Corrupt data Error in CPL_gdal_dimension(st_geometry(x), NA_if_empty) : OGR error"
  st_crs(growth_df) <- NA
  st_crs(buffer_boundary) <- NA

  buffer_index <- !st_intersects(growth_df, buffer_boundary, sparse = FALSE) %>%
    as.vector()

  growth_df <- growth_df %>%
    mutate(buffer = buffer_index)

  return(growth_df)
}


#' Define spatial crossvalidation grid
#'
#' @param forest data frame containing tree data
#' @param cv_fold_size grid size for spatial crossvalidation fold
#'
#' @return The same \code{forest} data frame but with a new numerical variable
#'   \code{fold} indicating the fold number.
#' @export
#' @import dplyr
#' @importFrom stringr str_c
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Define crossvalidation folds/grid
#' bigwoods <- bigwoods %>%
#'   define_cv_grid(cv_fold_size = 100) %>%
#'   mutate(fold = factor(fold))
#'
#' ggplot(data = bigwoods, aes(x = x, y = y)) +
#'   geom_point(aes(col = fold)) +
#'   coord_fixed(ratio = 1)
define_cv_grid <- function(forest, cv_fold_size){
  forest <- forest %>%
    mutate(
      x_fold = as.character(floor(x / cv_fold_size)),
      y_fold = as.character(floor(y / cv_fold_size)),
      fold = str_c("x", x_fold, "y", y_fold),
      fold = factor(fold, labels = c(1:n_distinct(fold))),
      fold = as.numeric(fold)
    ) %>%
    select(-c(x_fold, y_fold))

  return(forest)
}


#' Return information on each fold in crossvalidation grid
#'
#' @inheritParams define_cv_grid
#'
#' @return A "nested" dataframe with the fold number, its rough centroid, and
#'   all its neighbors in a nested list.
#' @export
#' @import dplyr
#' @importFrom tidyr nest
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' \dontrun{
#'
#' # Define crossvalidation folds/grid
#' bigwoods <- bigwoods %>%
#'   define_cv_grid(cv_fold_size = 100) %>%
#'   mutate(fold = factor(fold))
#'
#' # CV fold/grid info
#' folds <- bigwoods %>%
#'   get_cv_fold_info(cv_fold_size = 100)
#'
#' # Plot fold numbers
#' ggplot(data = bigwoods, aes(x = x, y = y)) +
#'   geom_point(aes(col = fold)) +
#'   coord_fixed(ratio = 1) +
#'   geom_text(data = folds, aes(x = x, y = y, label = fold), size = 10)
#'
#' # Check that neighbors are correct:
#' library(stringr)
#' library(tidyr)
#' neighbors <- folds %>%
#'   unnest() %>%
#'   group_by(fold) %>%
#'   summarize(neighbors = str_c(neighbor, collapse = ","))
#' neighbors
#' }
get_cv_fold_info <- function(forest, cv_fold_size){
  # Save output here
  fold_neighbors <- NULL

  for(i in 1:n_distinct(forest$foldID)){
    center_tree <- forest %>%
      filter(foldID == i) %>%
      summarise(x = mean(x), y = mean(y))

    fold_neighbors <- forest %>%
      filter(
        between(x, center_tree$x - cv_fold_size, center_tree$x + cv_fold_size),
        between(y, center_tree$y - cv_fold_size, center_tree$y + cv_fold_size)
      ) %>%
      pull(foldID) %>%
      unique() %>%
      sort() %>%
      data_frame(neighbor = .) %>%
      mutate(
        fold = i,
        x = center_tree$x,
        y = center_tree$y
      ) %>%
      select(foldID, x, y, neighbor) %>%
      nest(data=c(neighbor)) %>%
      bind_rows(fold_neighbors)
  }

  fold_neighbors <- fold_neighbors %>%
    arrange(foldID)
  return(fold_neighbors)
}

