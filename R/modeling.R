#' Define notion of species based on model choice
#'
#' @inheritParams define_cv_grid
#' @param model_number Number of model out of 1-3 from paper
#' @param species_notion Notion of grouping of individuals
#'
#' @import dplyr
#' @importFrom stats as.formula
#' @importFrom stringr str_c
#' @return A list of outputs
#' @export
#' @examples
#' 1+1
get_model_specs <- function(forest, model_number, species_notion){
  # Define 3 possible models for 3 notions of competition
  model_1_formula <-
    paste0("growth ~ ", species_notion, " + dbh + dbh * ", species_notion)

  model_2_formula <-
    paste0("growth ~ ", species_notion, " + dbh + dbh * ", species_notion, " + comp_basal_area + comp_basal_area * ", species_notion)

  model_3_formula <- forest %>%
    pull(species_notion) %>%
    unique() %>%
    sort() %>%
    paste0('`', ., '`') %>%
    paste(., "*", species_notion, sep = "", collapse = " + ") %>%
    paste(model_2_formula, '+', .)

  # Convert desied model to formula object:
  model_formula <- model_number %>%
    paste("model_", ., "_formula", sep="") %>%
    get() %>%
    as.formula()

  # Species of interest to model. These should be a subset of the species
  # correspoding to notion_of_focal_species.
  species_of_interest <- forest %>%
    pull(species_notion) %>%
    unique()

  # Return output list
  output <- list(
    model_formula = model_formula,
    notion_of_focal_species = species_notion,
    notion_of_competitor_species = species_notion,
    species_of_interest = species_of_interest
  )
  return(output)
}



#' Fit bayesian model
#'
#' @param focal_vs_comp from \code{\link{create_focal_vs_comp}}
#' @param run_shuffle boolean as to whether to run permutation test shuffle
#' @param prior_hyperparameters list of a0, b0, mu_0 and V_0
#' @inheritParams create_focal_vs_comp
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
fit_bayesian_model <- function(focal_vs_comp, model_specs, run_shuffle = FALSE,
                               prior_hyperparameters = NULL){

  # Get model formula
  model_formula <- model_specs$model_formula

  # Prepare data for regression Generate data frame of all focal trees
  focal_trees <- focal_vs_comp %>%
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID, comp_notion_of_species) %>%
    # Sum basal area & count of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(
      basal_area_total = sum(comp_basal_area),
      comp_basal_area = sum(comp_basal_area),
      n_comp = n()
    ) %>%
    arrange(focal_ID)

  # Shuffle group label only if flag is set
  if(run_shuffle){
    focal_trees <- focal_trees %>%
      group_by(focal_notion_of_species) %>%
      mutate(comp_notion_of_species = sample(comp_notion_of_species))
  }

  # Continue processing focal_trees
  focal_trees <- focal_trees %>%
    ungroup() %>%
    # sum biomass and n_comp for competitors of same species. we need to do this
    # for the cases when we do permutation shuffle.
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID, comp_notion_of_species) %>%
    summarise_all(list(sum)) %>%
    ungroup() %>%
    # compute biomass for each tree type
    pivot_wider(names_from = comp_notion_of_species, values_from = basal_area_total, values_fill = list(basal_area_total = 0)) %>%
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID) %>%
    summarise_all(list(sum)) %>%
    ungroup() %>%
    # sort by focal tree ID number
    arrange(focal_ID) %>%
    rename(!!model_specs$notion_of_focal_species := focal_notion_of_species)

  # Add biomass=0 for any species for which there are no trees
  species_levels <- model_specs$species_of_interest
  missing_species <- species_levels[!species_levels %in% names(focal_trees)] %>%
    as.character()
  if(length(missing_species) > 0){
    for(i in 1:length(missing_species)){
      focal_trees <- focal_trees %>%
        mutate(!!missing_species[i] := 0)
    }
    focal_trees <- focal_trees %>%
      select(everything(), !!species_levels)
  }

  # Matrix objects for analytic computation of all posteriors
  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)


  # Set priors ----------------------------------------------------------------
  # If no prior_hyperparameters specified
  if(is.null(prior_hyperparameters)){
    # Prior parameters for sigma2:
    a_0 <- 250
    b_0 <- 25

    # Prior parameters for betas and lambdas:
    mu_0 <- rep(0, ncol(X)) %>%
      matrix(ncol = 1)
    V_0 <- ncol(X) %>% diag()
  } else {
    a_0 <- prior_hyperparameters$a_0
    b_0 <- prior_hyperparameters$b_0
    mu_0 <- prior_hyperparameters$mu_0
    V_0 <- prior_hyperparameters$V_0
  }

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
  posterior_hyperparameters <- list(
    a_star = a_star,
    b_star = b_star,
    mu_star = mu_star,
    V_star = V_star
  )
  return(posterior_hyperparameters)
}


#' Make predictions based on bayesian model
#'
#' @inheritParams fit_bayesian_model
#' @param posterior_param Output of \code{\link{fit_bayesian_model}}
#' @inheritParams create_focal_vs_comp
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr nest
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1+1
predict_bayesian_model <- function(focal_vs_comp, model_specs, posterior_param){

  # Get model formula
  model_formula <- model_specs$model_formula

  # Prepare data for regression Generate data frame of all focal trees
  focal_trees <- focal_vs_comp %>%
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID, comp_notion_of_species) %>%
    # Sum basal area & count of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(
      basal_area_total = sum(comp_basal_area),
      comp_basal_area = sum(comp_basal_area),
      n_comp = n()
    ) %>%
    arrange(focal_ID)

  # Continue processing focal_trees
  focal_trees <- focal_trees %>%
    ungroup() %>%
    # sum biomass and n_comp for competitors of same species. we need to do this
    # for the cases when we do permutation shuffle.
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID, comp_notion_of_species) %>%
    summarise_all(list(sum)) %>%
    ungroup() %>%
    # compute biomass for each tree type
    pivot_wider(names_from = comp_notion_of_species, values_from = basal_area_total, values_fill = list(basal_area_total = 0)) %>%
    group_by(focal_ID, focal_notion_of_species, dbh, growth, foldID) %>%
    summarise_all(list(sum)) %>%
    ungroup() %>%
    # sort by focal tree ID number
    arrange(focal_ID) %>%
    rename(!!model_specs$notion_of_focal_species := focal_notion_of_species)

  # Add biomass=0 for any species for which there are no trees
  species_levels <- model_specs$species_of_interest
  missing_species <- species_levels[!species_levels %in% names(focal_trees)] %>%
    as.character()
  if(length(missing_species) > 0){
    for(i in 1:length(missing_species)){
      focal_trees <- focal_trees %>%
        mutate(!!missing_species[i] := 0)
    }
    focal_trees <- focal_trees %>%
      select(everything(), !!species_levels)
  }

  # Matrix objects for analytic computation of all posteriors
  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Make posterior predictions
  mu_star <- posterior_param$mu_star
  focal_trees <- focal_trees %>%
    mutate(growth_hat = as.vector(X %*% mu_star)) %>%
    select(focal_ID, growth_hat)

  # why do we return focal_vs_comp?? Shouldn't we return focal_trees (one row per focal tree not per interaction)
  return(focal_trees)
}


#' Run the bayesain model with spatial cross validation
#'
#' @inheritParams fit_bayesian_model
#' @param max_dist distance of competitive neighborhood
#' @param cv_grid length of the cross validation grid
#'
#' @import dplyr
#' @import sf
#' @import sfheaders
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1+1
run_cv <- function(focal_vs_comp, model_specs, max_dist, cv_grid,
                   run_shuffle = FALSE, prior_hyperparameters = NULL,
                   all_folds = TRUE){

  if(FALSE){
    # Code to test SCBI
    focal_vs_comp <- focal_vs_comp_scbi
    model_specs <- scbi_specs
    cv_grid <- scbi_cv_grid

    run_shuffle = FALSE
    prior_hyperparameters = NULL
    all_folds = FALSE


    # Code to test BigWoods
    focal_vs_comp <- focal_vs_comp_bw
    model_specs <- bw_specs
    cv_grid <- bw_cv_grid

    run_shuffle = FALSE
    prior_hyperparameters = NULL
    all_folds = TRUE
  }


  # if subset is true just two folds
  if (all_folds) {
    folds <- focal_vs_comp %>%
      pull(foldID) %>%
      unique() %>%
      sort()
  } else {
    folds <- c(23, 2)
  }

  # Store resulting y-hat for each focal tree here
  focal_trees <- tibble(focal_ID = NA, growth_hat  = NA)

  for (i in folds){
    # first pull out the train set and full test set
    train <- focal_vs_comp %>%
      filter(foldID != i)
    test_full <- focal_vs_comp %>%
      filter(foldID == i)

    if(FALSE){
      # Visualize original folds
      cv_grid$plots

      # View training set (slow)
      train %>% st_as_sf() %>% ggplot() + geom_sf()
    }

    # now buffer off the test fold by max_dist
    # sort of sloppy! But I think works
    test_fold <- cv_grid$blocks %>%
      subset(folds == i)

    test_fold_boundary <- test_fold %>%
      st_bbox() %>%
      st_as_sfc()

    test_fold_boundary_buffer <- test_fold_boundary %>%
      st_buffer(dist = -max_dist)

    test_fold_interior <- test_full %>%
      st_as_sf() %>%
      st_intersects(test_fold_boundary_buffer, sparse = FALSE)

    test <- test_full %>%
      filter(test_fold_interior)

    if(FALSE){
      # Visualize original folds
      cv_grid$plots

      # Visualize test set trees + boundary
      ggplot() +
        geom_sf(data = test_fold_boundary, col = "black") +
        geom_sf(data = test_fold_boundary_buffer, col = "red") +
        geom_sf(data = test_full %>% st_as_sf(), col = "black") +
        geom_sf(data = test %>% st_as_sf(), col = "red")
    }

    # now pretty easy to just call the two functions!
    fold_fit <- train %>%
      fit_bayesian_model(model_specs)
    fold_predict <- test %>%
      predict_bayesian_model(model_specs, fold_fit)

    # Append results
    focal_trees <- focal_trees %>%
      bind_rows(fold_predict) %>%
      filter(!is.na(focal_ID))
  }

  return(focal_trees)
}

#' Plot beta_0 parameters
#'
#' @inheritParams fit_bayesian_model
#' @param posterior_param Output of \code{\link{fit_bayesian_model}}
#'
#' @import ggridges
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1+1
plot_beta0 <- function(posterior_param, model_specs){

  # how we did it in the paper
  if (FALSE)
  {
    n_sim <- 100
    nu_star <- 2*posterior_param$a_star
    Sigma_star <- (posterior_param$b_star/posterior_param$a_star)*posterior_param$V_star

    beta_lambda_posterior_df <-
      rmvt(n_sim, sigma = Sigma_star, mu = as.vector(posterior_param$mu_star), df = nu_star) %>%
      data.frame() %>%
      as_tibble() %>%
      purrr::set_names(colnames(posterior_param$V_star)) %>%
      tidyr::gather(type, value)

    coefficient_types <- beta_lambda_posterior_df %>%
      select(type) %>%
      distinct() %>%
      mutate(
        coefficient_type =
          case_when(
            type == "(Intercept)" ~ "intercept",
            type %in% str_c("species", model_specs$species_of_interest) ~ "intercept",
            str_detect(type, "dbh") ~ "dbh",
            type %in% model_specs$species_of_interest ~ "competition",
            str_detect(type, ":") ~ "competition",
            # Need this for everything else that aren't the two cases above:
            TRUE ~ "NA"
          ))

    beta_lambda_posterior_df <- beta_lambda_posterior_df %>%
      left_join(coefficient_types, by = "type") %>%
      select(type, coefficient_type, value) %>%
      group_by(type, coefficient_type) %>%
      mutate(sim_ID = 1:n()) %>%
      select(sim_ID, everything()) %>%
      ungroup()

    baseline_species <- model_specs$species_of_interest[1] %>% as.character()

    posterior_sample <- beta_lambda_posterior_df %>%
      filter(coefficient_type == "intercept")

    posterior_sample_baseline <- posterior_sample %>%
      filter(type == "(Intercept)") %>%
      rename(offset = value) %>%
      select(sim_ID, offset)

    posterior_beta_0 <- posterior_sample %>%
      left_join(posterior_sample_baseline, by = "sim_ID") %>%
      mutate(
        offset = ifelse(type == "(Intercept)", 0, offset),
        value = value + offset,
        type = ifelse(type == "(Intercept)", baseline_species, str_sub(type, 8))
      ) %>%
      mutate(type = str_to_title(type)) %>%
      select(-offset)

    ggplot(posterior_beta_0, aes(x=value, y = fct_rev(type))) +
      geom_density_ridges() +
      geom_vline(xintercept = 0, linetype = "dashed") +
      xlim(c(-0.1,0.5)) +
      labs(
        x = expression(paste(beta[0], " (cm ",y^{-1},')')),
        y = model_specs$notion_of_competitor_species
      )
  }


  # do we have to sample from the distribution to make the curves?
  # since we know mu and sigma, can't we just plot the exact curves?
  # would this be faster?
  # I am having a hard time pulling them out from the posterior_param object
  # but here is my thought

  params <- tibble(species = c('Oak', 'Maple', 'Hickory', 'Cherry'),
                   mu = c(0.1, 0.05, 0.15, 0.075),
                   sigma = c(0.025, 0.015, 0.05, 0.03))

  x_lims <- params %>%
    mutate(max_val = mu + sigma*4,
           min_val = mu - sigma*4) %>%
    summarise(min_val = min(min_val), max_val = max(max_val)) %>%
    as.numeric()

  x_range_len <- 500
  x_range <- seq(from = x_lims[1], to = x_lims[2], length = x_range_len)

  posterior_curves <- tibble(species = rep(params$species[1], x_range_len),
                             x = x_range,
                             y = dnorm(x_range,params$mu[1],params$sigma[1]))

  # can you do this will purrr??
  for (sp in 2:dim(params)[1])
  {
    to_add <- tibble(species = rep(params$species[sp], x_range_len),
                               x = x_range,
                               y = dnorm(x_range,params$mu[sp],params$sigma[sp]))

    posterior_curves <- rbind(posterior_curves, to_add)
  }

  ggplot(posterior_curves, aes(x = x, y = species, height = y)) +
    geom_density_ridges(stat = 'identity') +
    geom_vline(xintercept = 0, linetype = "dashed")



}

