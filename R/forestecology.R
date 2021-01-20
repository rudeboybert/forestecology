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
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "dbh08", "dbh14", "family_phylo", "trait_group", "buffer", "code14",
      "species", "focal_ID", "ID", "spCode", "fold", "basal_area", "dbh",
      "focal_ID_neighbor", "focal_bw_ID", "comp_species", "comp_biomass",
      "inside", "growth", "biomass_total", "model_formula", "growth_hat",
      "comp_ID", "x", "y", "x_fold", "y_fold", "neighbor",
      "notion_of_competitor_species",
      "ExactDate", "ExactDate2", "code", "code_2018", "dbh1", "dbh2", "family",
      "growth_df", "gx", "gy", "n_days", "n_years", "sp", "stemID",
      ".", ":=", "comp_sp", "comp_basal_area", "focal_sp", "codes2", "foldID",
      "geometry", "folds", "dist", "foldID.y", "foldID.x", "geometry.y", "geometry.x",
      "Var1", "Var2", "aes", "as.formula", "codes2", "coefficient_type", "comp_basal_area",
      "comp_sp", "dist", "facet_wrap", "fct_rev", "focal_sp", "foldID", "foldID.x", "foldID.y",
      "folds", "gather", "geom_vline", "geometry", "geometry.x", "geometry.y", "ggplot", "labs",
      "offset", "sim_ID", "str_c", "str_sub", "type", "value", "values", "codes", "date1", "date2",
      "x_dir", "y_dir", "predict", "posterior_param"
    )
  )
}





#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
