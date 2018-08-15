#' Bigwoods forest data
#'
#' Census of trees in bigwoods forest
#'
#' @format A data frame with 50023 rows and 10 variables:
#' \describe{
#'   \item{ID}{Tree ID}
#'   \item{species}{Linnaean species name}
#'   \item{family_phylo}{Corresponding species phylogenetic family}
#'   \item{trait_group}{Corresponding species trait-based cluster}
#'   \item{x}{x-coordinate meters from reference point}
#'   \item{y}{y-coordinate meters from reference point}
#'   \item{growth}{Average annual growth in diameter at breast height from 2008-2014 6 years}
#'   \item{dbh08}{Diameter at breast high in 2008}
#'   \item{dbh14}{Diameter at breast high in 2014}
#'   \item{code14}{Code}
#' }
#' @seealso \code{\link{families}}
"bigwoods"


#' Bigwoods forest study region boundary
#'
#' Boundary region for bigwoods defined in terms of (x,y) vertices of a polygon.
#'
#' @format A data frame with 13 rows and 2 variables:
#' \describe{
#'   \item{x}{x-coordinate (meters from reference point)}
#'   \item{y}{y-coordinate (meters from reference point)}
#' }
#' @seealso \code{\link{families}}
#' @examples
#' library(ggplot2)
#' ggplot(data = bigwoods, aes(x,y)) +
#'   geom_point() +
#'   coord_fixed(ratio=1) +
#'   geom_path(data = bigwoods_study_region, col = "red", size = 1)
"bigwoods_study_region"



#' Phylogenic groupings and trait based clustering of various tree species
#'
#' A dataset mapping various tree species to their phylogenetic families and
#' trait-based (rather than evolutionary relationship-based) clusters.
#'
#' @format A data frame with 50 rows and 3 variables:
#' \describe{
#'   \item{spcode}{Linnaean species name}
#'   \item{family_phylo}{Taxonomic/phylogenetic grouping at higher level than family.}
#'   \item{trait_group}{Clustering of species based on their traits rather than
#'   their evolutionary relationships.}
#' }
#' @source See \url{https://en.wikipedia.org/wiki/Family_(biology)} for
#'   biological/taxonomical definition of family.
#' @seealso \code{\link{bigwoods}}
"families"


#' Create main data frame for analysis
#'
#' @inheritParams define_cv_grid
#' @inheritParams define_bigwoods_buffer
#' @param folds CV fold information from \code{\link{define_cv_grid}}
#' @param model_specs from \code{\link{get_model_specs}}
#' @return \code{focal_vs_comp} data frame
#' @export
#' @import dplyr
#' @importFrom proxy dist
#' @seealso \code{\link{define_cv_grid}} and \code{\link{get_model_specs}}
#' @examples
#' 1+1
create_focal_vs_comp <- function(forest, max_dist, folds, model_specs){

  # Extract model specifications
  notion_of_focal_species <- model_specs$notion_of_focal_species
  notion_of_competitor_species <- model_specs$notion_of_competitor_species
  model_formula <- model_specs$model_formula
  species_of_interest <- model_specs$species_of_interest

  # 1. Define focal trees where notion of "species" depends on
  # notion_of_focal_species
  focal_trees <- forest %>%
    # Define notion of speices
    mutate_(species = notion_of_focal_species) %>%
    select(-c(family_phylo, trait_group)) %>%
    # Only alive both dates, not in buffer
    filter(dbh08>0, dbh14>0, !buffer, code14 != 'R') %>%
    rename(dbh = dbh08) %>%
    # Assign species numerical code.
    mutate(spCode = as.numeric(factor(species))) %>%
    # ID numbers to join focal trees with competitor trees
    mutate(focal_ID = 1:n()) %>%
    select(focal_ID, ID, species, spCode, x, y, dbh, growth, fold)

  # 2. Define competitor trees where notion of "species" depends on
  # notion_of_competitor_species
  comp_trees <- forest %>%
    # Define notion of speices
    mutate_(species = notion_of_competitor_species) %>%
    select(-c(family_phylo,trait_group)) %>%
    # Only species alive in 2008
    filter(dbh08>0) %>%
    rename(dbh = dbh08) %>%
    mutate(
      comp_ID = 1:n(),
      basal_area = dbh^2*0.00007854
    ) %>%
    select(comp_ID, ID, species, x, y, dbh, basal_area, fold)


  # 2. Define distances of focal trees to other focal trees.
  # Note: This will be used to determine which trees to exclude to break residual
  # spatial auto-correlation in cross-validation algorithm. We only need distance
  # Compute focal_vs_focal for the particular parameter setting.
  # Note however for the exhaustive/slowest case, the code below took ~11s
  # notion_of_focal_species == "species"
  # notion_of_competitor_species == "species"
  # species_of_interest == unique(bw$species)
  focal_vs_focal <- NULL
  for(i in 1:max(forest$fold)){
    x <- focal_trees %>%
      filter(fold == i)
    y <- focal_trees %>%
      filter(fold %in% unlist(folds$data[folds$fold == i]))

    D <- proxy::dist(x=x[, c("x", "y")], y=y[, c("x", "y")])

    focal_vs_focal_fold_i <- data_frame(
      focal_ID = rep(x$focal_ID, each=nrow(y)),
      focal_ID_neighbor = rep(y$focal_ID, times=nrow(x)),
      dist = as.vector(t(D))
    ) %>%
      filter(dist < max_dist) %>%
      group_by(focal_ID)

    focal_vs_focal <- focal_vs_focal %>%
      bind_rows(focal_vs_focal_fold_i)
  }
  # Sort
  focal_vs_focal <- focal_vs_focal %>%
    arrange(focal_ID, focal_ID_neighbor)

  # 3. Define distances from focal trees to competitor trees.
  # Note: This contains all focal tree vs competitor tree information!
  # We have focal_vs_comp cases saved for both
  # -notion_of_competitor_species = "species"
  # -notion_of_competitor_species = "first_order"
  # If all parameter settings match the saved case, load the file
  #
  # Compute focal_vs_comp for the particular parameter setting.
  # Note however for the exhaustive/slowest case, the code below took ~17s
  # notion_of_focal_species == "species"
  # notion_of_competitor_species == "species"
  # species_of_interest == unique(bw$species)
  focal_vs_comp <- NULL
  for(i in 1:max(forest$fold)){
    x <- focal_trees %>%
      filter(fold == i)
    y <- comp_trees %>%
      filter(fold %in% unlist(folds$data[i]))

    D <- proxy::dist(x=x[, c("x", "y")], y=y[, c("x", "y")])

    focal_vs_comp_fold_i <- data_frame(
      focal_ID = rep(x$focal_ID, each=nrow(y)),
      focal_bw_ID = rep(x$ID, each=nrow(y)),
      comp_ID = rep(y$comp_ID, times=nrow(x)),
      dist = as.vector(t(D))
    ) %>%
      filter(dist < max_dist) %>%
      left_join(comp_trees, by="comp_ID") %>%
      # Focal trees cannot be competitors with themselves. Remove these cases:
      filter(focal_bw_ID != ID) %>%
      group_by(focal_ID) %>%
      select(-c(x, y, focal_bw_ID, ID, fold))

    focal_vs_comp <- focal_vs_comp %>%
      bind_rows(focal_vs_comp_fold_i)
  }

  focal_vs_comp <- focal_vs_comp %>%
    arrange(focal_ID, comp_ID) %>%
    rename(
      comp_species = species,
      comp_dbh = dbh,
      comp_biomass = basal_area
    ) %>%
    left_join(focal_trees, by = "focal_ID") %>%
    select(
      ID,
      focal_ID, species, spCode, x, y, dbh, growth, fold,
      comp_species, comp_biomass, #comp_ID, dist, comp_dbh,
    ) %>%
      group_by(ID, focal_ID, species, spCode, x, y, dbh, growth, fold) %>%
    nest(.key = "comp")

  return(focal_vs_comp)
}

