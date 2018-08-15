#' Define notion of species based on model choice
#'
#' @inheritParams define_cv_grid
#' @param model_number Number of model out of 1-5 from paper
#'
#' @import dplyr
#' @importFrom stats as.formula
#' @return A list of outputs
#' @export
#' @examples
#' 1+1
get_model_specs <- function(forest, model_number){
  # Define model formula to use out of 5 possible choices defined in paper
  # TODO: Generalize this for any species/family list
  model_1_formula <-
    "growth ~ species + dbh + dbh*species"

  model_2_formula <-
    "growth ~ species + dbh + dbh*species + biomass + biomass*species"

  model_3_formula <- forest$family_phylo %>%
    unique() %>%
    sort() %>%
    paste(., "*species", sep = "", collapse = " + ") %>%
    paste("growth ~ species + dbh + dbh*species + ", .)

  model_4_formula <- forest$trait_group %>%
    unique() %>%
    sort() %>%
    paste(., "*species", sep = "", collapse = " + ") %>%
    paste("growth ~ species + dbh + dbh*species + ", .)

  model_5_formula <- forest$species %>%
    unique() %>%
    sort() %>%
    paste(., "*species", sep = "", collapse = " + ") %>%
    paste("growth ~ species + dbh + dbh*species + ", .)

  model_formula <- model_number %>%
    paste("model_", ., "_formula", sep="") %>%
    get() %>%
    as.formula()

  # Notion of species for both focal and competitor trees. Either:
  # + "species" (46 levels) or
  # + "family_phylo" (23 levels) or
  # + "trait_group" (6 levels)
  notion_of_spp_indexer <-
    c(rep("species", 2), "family_phylo", "trait_group", "species")
  notion_of_focal_species <- notion_of_spp_indexer[model_number]
  notion_of_competitor_species <- notion_of_spp_indexer[model_number]

  # Species of interest to model. These should be a subset of the species
  # correspoding to notion_of_focal_species.
  species_of_interest <- forest %>%
    select_(notion_of_focal_species) %>%
    distinct() %>%
    pull()

  # Return output list
  output <- list(
    model_formula = model_formula,
    notion_of_focal_species = notion_of_focal_species,
    notion_of_competitor_species = notion_of_competitor_species,
    species_of_interest = species_of_interest
  )
  return(output)
}



#' Fit bayesian model
#'
#' @param focal_vs_comp from \code{\link{create_focal_vs_comp}}
#' @param run_shuffle boolean
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#' @return Posterior parameter values
#' @export
#'
#' @examples
#' 1+1
fit_bayesian_model <- function(focal_vs_comp, run_shuffle = FALSE){
  # Prepare data for regression Generate data frame of all focal trees
  focal_trees <- focal_vs_comp %>%
    unnest() %>%
    group_by(ID, focal_ID, species, spCode, x, y, dbh, growth, fold, comp_species) %>%
    # Sum biomass & count of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(
      biomass_total = sum(comp_biomass),
      biomass = sum(comp_biomass),
      n_comp = n()
    ) %>%
    arrange(focal_ID)

  # Shuffle group label only if flag is set
  if(run_shuffle){
    focal_trees <- focal_trees %>%
      group_by(focal_ID) %>%
      mutate(comp_species = sample(comp_species)) %>%
      group_by(ID, focal_ID, species, spCode, x, y, dbh, growth, fold, comp_species)
  }

  # Continue processing focal_trees_new
  focal_trees <- focal_trees %>%
    spread(comp_species, biomass_total, fill = 0) %>%
    group_by(ID, focal_ID, species, spCode, x, y, dbh, growth, fold) %>%
    summarise_all(funs(sum)) %>%
    arrange(focal_ID)

  # Matrix objects for analytic computation of all posteriors
  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Set priors ----------------------------------------------------------------
  # Prior parameters for sigma2:
  a_0 <- 250
  b_0 <- 25

  # Prior parameters for betas and lambdas:
  mu_0 <- rep(0, ncol(X)) %>%
    matrix(ncol = 1)
  V_0 <- ncol(X) %>% diag()


  # Compute posteriors --------------------------------------------------------
  # Posterior parameters for betas and lambdas:
  mu_star <- solve(solve(V_0) + t(X) %*% X) %*% (solve(V_0) %*% mu_0 + t(X) %*% y)
  V_star <- solve(solve(V_0) + t(X) %*% X)

  # Posterior parameters for sigma2
  a_star <- a_0 + n/2
  b_star <- b_0 + 0.5 * (
    t(mu_0) %*% solve(V_0) %*% mu_0 +
      t(y) %*% y -
      t(mu_star) %*% solve(V_star) %*% mu_star
  ) %>%
    as.vector()

  # Make posterior predictions
  # focal_trees_new <- focal_trees_new %>%
  #   mutate(growth_hat = as.vector(X %*% mu_star))

  # Return posterior parameters
  list(
    mu_star = mu_star,
    V_star = V_star,
    a_star = a_star,
    b_star = b_star
  ) %>%
    return()
}


#' Make predictions based on bayesian model
#'
#' @inheritParams fit_bayesian_model
#' @param posterior_param Output of \code{\link{fit_bayesian_model}}
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr nest
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1+1
predict_bayesian_model <- function(focal_vs_comp, posterior_param){
  # Prepare data for regression Generate data frame of all focal trees
  focal_trees <- focal_vs_comp %>%
    unnest() %>%
    group_by(ID, focal_ID, species, spCode, x, y, dbh, growth, fold, comp_species) %>%
    # Sum biomass & count of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(
      biomass_total = sum(comp_biomass),
      biomass = sum(comp_biomass),
      n_comp = n()
    ) %>%
    arrange(focal_ID)

  # Continue processing focal_trees_new
  focal_trees <- focal_trees %>%
    spread(comp_species, biomass_total, fill = 0) %>%
    group_by(ID, focal_ID, species, spCode, x, y, dbh, growth, fold) %>%
    summarise_all(funs(sum)) %>%
    arrange(focal_ID)

  # Matrix objects for analytic computation of all posteriors
  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Make posterior predictions
  mu_star <- posterior_param$mu_star
  focal_vs_comp <- focal_vs_comp %>%
    mutate(growth_hat = as.vector(X %*% mu_star)) %>%
    select(ID, focal_ID, species, spCode, x, y, dbh, growth, growth_hat, fold, everything())

  return(focal_vs_comp)
}
