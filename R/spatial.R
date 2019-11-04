#' Identify which trees are in buffer region of bigwoods.
#'
#' @param bigwoods \code{\link{bigwoods}} data frame
#' @param max_dist Maximum distance from a focal tree for another tree to be
#'   considered a competitor tree
#' @return The same \code{\link{bigwoods}} data frame but with a new boolean
#'   variable \code{buffer} indicating if a tree is in the study region buffer
#'   area. This uses \code{\link{bigwoods_study_region}} to define the boundary:
#' @export
#' @import dplyr
#' @importFrom sp point.in.polygon
#' @importFrom concaveman concaveman
#' @seealso \code{\link{bigwoods}}
#'
#' @examples
#' library(ggplot2)
#'
#' # Create buffer variable:
#' bigwoods <- bigwoods %>%
#'   define_bigwoods_buffer(max_dist = 7.5)
#'
#' ggplot(bigwoods, aes(x = x, y = y)) +
#'   # Mark study region boundary
#'   geom_path(data = bigwoods_study_region, size = 1) +
#'   # Mark buffer vs non-buffer trees:
#'   geom_point(aes(col = buffer)) +
#'   coord_fixed(ratio = 1) +
#'   labs(title = "Study region boundary and buffer region")
define_bigwoods_buffer <- function(bigwoods, max_dist){
  bounding_points <- concaveman::concaveman(matrix(c(bigwoods$x,bigwoods$y),ncol=2))
  bounding_points <- tibble(x= bounding_points[,1],y=bounding_points[,2])
  centroid <- bounding_points %>%
    summarise(x = mean(x), y= mean(y) )
  bounding_points <- bounding_points %>%
    mutate(x_dir = ifelse(x-centroid$x>0,-1,1),
           y_dir = ifelse(y-centroid$y>0,-1,1),
           x_buff = x + max_dist*x_dir,
           y_buff = y + max_dist*y_dir)

  x_buff <- bounding_points$x_buff
  y_buff <- bounding_points$y_buff

  bigwoods <- bigwoods %>%
    mutate(
      inside = sp::point.in.polygon(x,y,x_buff,y_buff),
      buffer = ifelse(inside == 0, TRUE, FALSE)
    ) %>%
    select(-inside)

  return(bigwoods)
}


#' Define spatial crossvalidation grid
#'
#' @inheritParams define_bigwoods_buffer
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
get_cv_fold_info <- function(forest, cv_fold_size){
  # Save output here
  fold_neighbors <- NULL

  for(i in 1:n_distinct(forest$fold)){
    center_tree <- forest %>%
      filter(fold == i) %>%
      summarise(x = mean(x), y = mean(y))

    fold_neighbors <- forest %>%
      filter(
        between(x, center_tree$x - cv_fold_size, center_tree$x + cv_fold_size),
        between(y, center_tree$y - cv_fold_size, center_tree$y + cv_fold_size)
      ) %>%
      pull(fold) %>%
      unique() %>%
      sort() %>%
      data_frame(neighbor = .) %>%
      mutate(
        fold = i,
        x = center_tree$x,
        y = center_tree$y
      ) %>%
      select(fold, x, y, neighbor) %>%
      nest(neighbor) %>%
      bind_rows(fold_neighbors)
  }

  fold_neighbors <- fold_neighbors %>%
    arrange(fold)
  return(fold_neighbors)
}

