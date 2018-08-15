#' Identify which trees are in buffer region of bigwoods.
#'
#' @param bigwoods \code{\link{bigwoods}} data frame
#' @param max_dist Maximum distance from a focal tree for another tree to be
#'   considered a competitor tree
#' @return The same \code{\link{bigwoods}} data frame but with a new boolean
#'   variable \code{buffer} indicating if a tree is in the study region buffer
#'   area.
#' @export
#' @import dplyr
#' @importFrom sp point.in.polygon
#' @seealso \code{\link{bigwoods}}
#'
#' @examples
#' 1+1
define_bigwoods_buffer <- function(bigwoods, max_dist){
  bigwoods_study_region <- bigwoods_study_region %>%
    # Define boundary: which trees fall within max_dist of boundary
    # TODO: Need to generalize this
    mutate(
      x_buff = x + max_dist * c(1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, 1),
      y_buff = y + max_dist * c(1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1)
    )

  bigwoods <- bigwoods %>%
    mutate(
      inside = sp::point.in.polygon(x, y, bigwoods_study_region$x_buff,
                                    bigwoods_study_region$y_buff),
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
#' max_dist <- 7.5
#' cv_fold_size <- 100
#' bigwoods <- bigwoods %>%
#'   define_bigwoods_buffer(max_dist) %>%
#'   define_cv_grid(cv_fold_size)
#'
#' ggplot(data = bigwoods, aes(x = x, y = y)) +
#'   geom_point(aes(col = factor(fold))) +
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
#' 1+1
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
