globalVariables(c(
  "codes", "date1", "date2"
))

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
#' @return growth_df A data frame with \code{growth}: average annual growth in dbh.
#' @export
#' @import dplyr
#' @examples
#' 1+1
compute_growth <- function(census_df1, census_df2, id) {

  # TODO: Write following checks
  # - Both census data frames have variables: id, dbh, date, and codes.
  # - Check variable types: chr, dbl, date/dttm, NA
  # - Check that id uniquely identifies rows
  # - Prompt use with message: "Assuming dbh are in cm"

  # Limit second census data to only those variables that can change
  census_df2 <- census_df2 %>%
    select(id, dbh, date, codes)

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
      n_years = n_days/365.25,
      growth = (dbh2 - dbh1)/n_years
    ) %>%
    select(-c(n_days, n_years, date1, date2))

  return(growth_df)
}


#' Create main data frame for analysis
#'
#' @inheritParams define_cv_grid
#' @inheritParams define_bigwoods_buffer
#' @param model_specs from \code{\link{get_model_specs}}
#' @return \code{focal_vs_comp} data frame
#' @export
#' @import dplyr
#' @importFrom proxy dist
#' @seealso \code{\link{define_cv_grid}} and \code{\link{get_model_specs}}
#' @examples
#' 1+1
create_focal_vs_comp <- function(growth_df, max_dist, model_specs, cv_grid, id, plot_folds = FALSE){
  # Extract model specifications
  notion_of_focal_species <- model_specs$notion_of_focal_species
  notion_of_competitor_species <- model_specs$notion_of_competitor_species

  # 1. Define focal trees where notion of "species" depends on
  # notion_of_focal_species
  growth_df_focal_trees <- growth_df %>%
    # Define notion of species as factor
    mutate(
      focal_notion_of_species = .data[[notion_of_focal_species]],
      focal_notion_of_species = factor(focal_notion_of_species)
    ) %>%
    # Only trees alive both dates, not in buffer, and not a resprout at 2nd
    # census (OK to be resprout at first census):
    filter(dbh1 > 0, dbh2 > 0, !buffer, codes2 != 'R') %>%
    rename(dbh = dbh1) %>%
    # Assign species numerical code.
    # mutate(spCode = as.numeric(notion_of_species)) %>%
    # ID numbers to join focal trees with competitor trees
    mutate(focal_ID = .data[[id]]) %>%
    select(focal_ID, foldID, geometry, growth, focal_notion_of_species, dbh)

  # 2. Define competitor trees where notion of "species" depends on
  # notion_of_competitor_species
  growth_df_comp_trees <- growth_df %>%
    # Define notion of speices
    mutate(
      comp_notion_of_species = .data[[notion_of_competitor_species]],
      comp_notion_of_species = factor(comp_notion_of_species)
    ) %>%
    # Only trees alive at first census:
    filter(dbh1 > 0) %>%
    rename(dbh = dbh1) %>%
    mutate(
      comp_ID = .data[[id]],
      # This assumes dbh is in cm, the resulting basal area will be in meters^2
      # https://en.wikipedia.org/wiki/Basal_area
      comp_basal_area = 0.0001 * pi * (dbh/2)^2
    ) %>%
    select(comp_ID, foldID, comp_notion_of_species, comp_basal_area)


  # 3. Define distances of focal trees to other focal trees.
  # Note: This will be used to determine which trees to exclude to break residual
  # spatial auto-correlation in cross-validation algorithm. We only need distance
  # Compute focal_vs_focal for the particular parameter setting.
  # Note however for the exhaustive/slowest case, the code below took ~11s
  # notion_of_focal_species == "species"
  # notion_of_competitor_species == "species"
  # species_of_interest == unique(bw$species)
  focal_vs_comp <- NULL
  all_folds <- growth_df_focal_trees %>%
    pull(foldID) %>%
    unique() %>%
    sort()

  for(i in 1:length(all_folds)){

    # Identify focal and competitor trees in this fold
    growth_df_focal_trees_current_fold <- growth_df_focal_trees %>%
      filter(foldID == all_folds[i])
    growth_df_comp_trees_current_fold <- growth_df_comp_trees %>%
      filter(foldID == all_folds[i])

    # Narrow down focal trees to those max_dist away (towards inside) from boundary
    current_fold_boundary <- cv_grid$blocks %>%
      st_as_sf() %>%
      filter(folds == all_folds[i])
    current_fold_inside <- current_fold_boundary %>%
      st_buffer(dist = -max_dist)

    inside_index <- st_intersects(growth_df_focal_trees_current_fold, current_fold_inside, sparse = FALSE)
    growth_df_focal_trees_current_fold <- growth_df_focal_trees_current_fold %>%
      mutate(inside = as.vector(inside_index))

    # Sanity check plot: for this fold, smaller black dots are competitor trees
    # and cyan larger dots are the test set. orange ones separating test set
    # from training set (trees in all other folds)
    if(plot_folds){
      plot_title <- str_c(
        "fold ", all_folds[i],
        ": Small black dots = competitor, cyan dots = test set, orange dots = buffer"
      )

      ggplot() +
        geom_sf(data = bw_boundary, col = "black") +
        geom_sf(data = current_fold_boundary, col = "black") +
        geom_sf(data = current_fold_inside, col = "red") +
        geom_sf(data = growth_df_focal_trees_current_fold, aes(col = inside), size = 3) +
        geom_sf(data = growth_df_comp_trees_current_fold, col = "blue", size = 0.5) +
        scale_color_manual(values = c("orange", "cyan")) +
        labs(title = plot_title)

      ggsave(str_c("cv_folds_sanity_check/fold_", all_folds[i], ".png"), width = 16, height = 9)
    }

    # Define data frame of distances
    x <- growth_df_focal_trees_current_fold
    y <- growth_df_comp_trees_current_fold
    distance_matrix <- st_distance(x, y)

    focal_vs_comp_current_fold <-
      # Convert distance matrix to vector along with ID's
      tibble(
        focal_ID = rep(x$focal_ID, each = nrow(y)),
        comp_ID = rep(y$comp_ID, times = nrow(x)),
        dist = distance_matrix %>% t() %>% as.vector()
      ) %>%
      # Remove pairs more than max_dist apart
      filter(dist < max_dist) %>%
      # Remove
      filter(focal_ID != comp_ID) %>%
      # Join focal tree data
      left_join(growth_df_focal_trees_current_fold, by = "focal_ID") %>%
      # Join competitor tree data
      left_join(growth_df_comp_trees_current_fold, by = "comp_ID") %>%
      # Clean up mess from join:
      select(-c(foldID.y, geometry.y, inside)) %>%
      rename(foldID = foldID.x, geometry = geometry.x)

    focal_vs_comp <- focal_vs_comp %>%
      rbind(focal_vs_comp_current_fold)
  }

  focal_vs_comp <- focal_vs_comp %>%
    arrange(focal_ID, comp_ID) %>%
    mutate(growth_hat = NA) %>%
    select(everything(), growth, growth_hat) %>%
    st_as_sf() %>%
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID,
             comp_notion_of_species)

  return(focal_vs_comp)
}
