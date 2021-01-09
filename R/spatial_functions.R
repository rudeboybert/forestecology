#' Compute buffer to a region.
#'
#' @inheritParams add_buffer_variable
#' @param region An \code{sf} polygon object of region to be buffered
#' @param direction "in" for buffers that are contained within \code{region}, "out" for buffers that contain \code{region}.
#' @return An \code{sf} polygon object of buffer
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
compute_buffer_region <- function(region, direction = "in", size) {
  # - Q: Force user to specify region as sf instead of just tibble object?
  # - Run tests on direction and size

  if (direction == "in") {
    size <- -1 * abs(size)
  } else if (direction == "out") {
    size <- abs(size)
  }

  buffer_region <- region %>%
    # sf_polygon() %>%
    st_buffer(dist = size)

  return(buffer_region)
}





#' Identify trees in the buffer region
#'
#' @param growth_df \code{sf} data frame
#' @param size Distance to determine which neighboring trees to a focal tree are competitors.
#' @inheritParams compute_buffer_region
#' @return The same \code{growth_df} data frame but with a new boolean
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
add_buffer_variable <- function(growth_df, direction, size, region) {
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





#' Return all pairwise distances between two data frames of trees
#'
#' @param focal_trees An \code{sf} polygon object of the focal trees of interest
#' @param comp_trees An \code{sf} polygon object of the competitor trees
#' @return A data frame with three columns: \code{focal_ID} of focal tree,
#'   \code{comp_dist} of competitor tree, and \code{dist} of distance between
#'   them.
#' @export
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' library(sf)
#'
#' # Create toy example focal and competitor trees
#' focal_trees <- tibble(
#'   focal_ID = c(1, 2, 3),
#'   x = c(0.3, 0.6, 0.7),
#'   y = c(0.1, 0.5, 0.7)
#' ) %>%
#'   st_as_sf(coords = c("x", "y"))
#'
#' comp_trees <- tibble(
#'   comp_ID = c(4, 5, 6, 7),
#'   x = c(0, 0.2, 0.4, 0.6),
#'   y = c(0.6, 0.7, 1, 0.2)
#' ) %>%
#'   st_as_sf(coords = c("x", "y"))
#'
#' # Plot both sets of trees
#' ggplot() +
#'   geom_sf_label(data = focal_trees, aes(label = focal_ID), col = "black") +
#'   geom_sf_label(data = comp_trees, aes(label = comp_ID), col = "orange") +
#'   labs(title = "Focal trees in black, competitor trees in orange")
#'
#' # Compute corresponding distances between the 3 focal trees and 4 competitor trees
#' focal_vs_comp_distance(focal_trees, comp_trees)
focal_vs_comp_distance <- function(focal_trees, comp_trees) {
  # Get IDs
  focal_IDs <- focal_trees$focal_ID
  comp_IDs <- comp_trees$comp_ID

  # Compute distance matrix
  distance_matrix <- comp_trees %>%
    st_distance(focal_trees)

  # Assign row and column names
  colnames(distance_matrix) <- focal_IDs
  rownames(distance_matrix) <- comp_IDs

  focal_vs_comp <-
    # Convert distance matrix to vector along with ID's
    tibble(
      focal_ID = rep(focal_IDs, each = length(comp_IDs)),
      comp_ID = rep(comp_IDs, times = length(focal_IDs)),
      dist = distance_matrix %>% as.vector()
    ) %>%
    # Remove cases where focal = comp
    filter(focal_ID != comp_ID)

  return(focal_vs_comp)
}
