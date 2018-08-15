#' \code{forestecology} package
#'
#' Methods and data for forest ecology model selection and assessment
#'
#' See the README on
#' \href{https://github.com/rudeboybert/forestecology#readme}{GitHub}
#'
#' @docType package
#' @name forestecology
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c("dbh08", "dbh14", "family_phylo", "trait_group", "buffer", "code14",
      "species", "focal_ID", "ID", "spCode", "fold", "basal_area", "dbh",
      "focal_ID_neighbor", "focal_bw_ID", "comp_species", "comp_biomass",
      "inside", "growth", "biomass_total", "model_formula", "growth_hat",
      "comp_ID", "x", "y", "x_fold", "y_fold", "neighbor", ".",
      "notion_of_competitor_species"
      ))
