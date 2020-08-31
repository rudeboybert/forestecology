#' Create focal versus competitor trees data frame
#'
#' @inheritParams compute_buffer_region
#' @param growth_df A \code{\link{compute_growth}} output converted to \linkS4class{sf} object
#' @param cv_grid_sf An sf object of a \code{blockCV} block output
#' @param id A character string of the variable name in \code{growth_df} uniquely identifying each tree
#' @return \code{focal_vs_comp} data frame of all focal trees and for each focal
#'   tree all possible competitor trees. In particular, for each competitor tree
#'   we compute the \href{https://en.wikipedia.org/wiki/Basal_area}{basal area}
#'   (in meters-squared) based on the `dbh1` variable from the first census
#'   (assumed to be in cm).
#' @export
#' @import dplyr
#' @description "Focal versus competitor trees" data frames are the main data
#'   frame used for analysis. "Focal trees" are all trees that satisfy the
#'   following criteria
#' \tabular{l}{
#'   1. Were alive at both censuses \cr
#'   2. Were not part of the study region's buffer as computed by \code{\link{add_buffer_variable}} \cr
#'   3. Were not a resprout at the second census. Such trees should be coded as `"R"` in the `codes2` variable (OK if a resprout at first census)
#' }
#' For each focal tree, "competitor trees" are all trees that (1) were alive at the first census and (2) within \code{max_dist} distance of the focal tree.
#' @note In order to speed computation, in particular of distances between all
#'   focal/competitor tree pairs, we use the cross-validation \code{blockCV}
#'   object to divide the study region into smaller subsets.
#' @seealso \code{\link{focal_vs_comp_distance}}
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(sf)
#' library(sfheaders)
#'
#' # Create fold information sf object. TODO: clean this
#' cv_grid_ex <-
#'   tibble(
#'     # Study region boundary
#'     x = c(0, 0, 5, 5),
#'     y = c(0, 5, 5, 0)
#'   ) %>%
#'   # Convert to sf object
#'   sf_polygon() %>%
#'   mutate(folds = "1")
#'
#' # Plot example data. Observe for max_dist = 1.5, there are 6 focal vs comp pairs:
#' # 1. focal 1 vs comp 2
#' # 2. focal 2 vs comp 1
#' # 3. focal 2 vs comp 3
#' # 4. focal 3 vs comp 2
#' # 5. focal 4 vs comp 5
#' # 6. focal 5 vs comp 4
#' ggplot() +
#'   geom_sf(data = cv_grid_ex, fill = "transparent") +
#'   geom_sf_label(data = growth_df_ex, aes(label = ID))
#'
#' # Return corresponding data frame
#' growth_df_ex %>%
#'   create_focal_vs_comp(max_dist = 1.5, cv_grid = cv_grid_ex, id = "ID")
create_focal_vs_comp <- function(growth_df, max_dist, cv_grid_sf, id){
  # TODO: Create example for this function using toy dataset
  # TODO: Inputs checks that growth_df has sp variable, maybe id variable

  # 1. Define focal trees
  focal_trees <- growth_df %>%
    # Identify trees that satisfy focal tree criteria
    filter(dbh1 > 0, dbh2 > 0, !buffer, codes2 != "R") %>%
    # Define notion of species as factor
    rename(dbh = dbh1) %>%
    # ID numbers to join focal trees with competitor trees
    mutate(focal_ID = .data[[id]]) %>%
    select(focal_ID, foldID, geometry, growth, focal_sp = sp, dbh)


  # 2. Define set of candidate competitor trees:
  comp_trees <- growth_df %>%
    # Identify trees that satisfy competitor tree criteria
    filter(dbh1 > 0) %>%
    mutate(
      comp_ID = .data[[id]],
      # Compute basal area using dbh1 from first census
      comp_basal_area = 0.0001 * pi * (dbh1/2)^2
    ) %>%
    select(comp_ID, foldID, comp_sp = sp, comp_basal_area)


  # 3. For each focal tree, identify all candidate competitor trees that are
  # within max_dist distance of it, and save as data frame. Note that to we do
  # this fold-by-fold using the previously computed blockCV grid object. We do
  # this to acceleration computation, in particular all distance pairs.
  all_folds <- focal_trees$foldID %>%
    unique()
  focal_vs_comp <- vector(mode = "list", length = length(all_folds))

  for(i in 1:length(all_folds)){
    # Identify this fold's boundary
    fold_boundary <- cv_grid_sf %>%
      filter(folds == all_folds[i])

    # Identify focal trees in this fold
    focal_trees_fold <- focal_trees %>%
      filter(foldID == all_folds[i])

    # Identify comp trees in this fold: both trees inside fold and those within
    # max_dist distance outwards of fold boundary
    comp_trees_fold <- comp_trees %>%
      add_buffer_variable(direction = "out", size = max_dist, region = fold_boundary) %>%
      filter(!buffer)

    if(FALSE){
      # Sanity check plot: for the ith fold, smaller black dots are competitor
      # trees and cyan larger dots are the test set. orange ones separating test
      # set from training set (trees in all other folds)
      ggplot() +
        # Focal & competitor trees:
        geom_sf(data = comp_trees_fold, col = "orange", size = 3, ) +
        geom_sf(data = focal_trees_fold, col = "black", size = 1, fill = "transparent") +
        # Boundaries
        # geom_sf(data = bw_study_region, col = "black", fill = "transparent", linetype = "dashed") +
        geom_sf(data = fold_boundary, col = "black", fill = "transparent") +
        labs(
          title = str_c("Fold ", all_folds[i], ": Focal = black, competitor = orange")
        )
    }

    # Save current fold info
    focal_vs_comp[[i]] <-
      # Take focal trees in this fold
      focal_trees_fold %>%
      # Compute distances to all competitor trees in this fold
      focal_vs_comp_distance(comp_trees_fold) %>%
      # Remove pairs more than max_dist apart
      filter(dist < max_dist) %>%
      # join focal tree data
      left_join(focal_trees_fold, by = "focal_ID") %>%
      # Join competitor tree data
      left_join(comp_trees_fold, by = "comp_ID") %>%
      # Clean up mess from join:
      select(-c(foldID.y, geometry.y)) %>%
      rename(foldID = foldID.x, geometry = geometry.x)
  }


  # 4. Return output data frame
  # TODO: Questions to consider
  # 1. Should we make this a nested-list object?
  # 2. Should we convert to sf object using st_as_sf() here?
  focal_vs_comp <- focal_vs_comp %>%
    # Convert list to tibble:
    bind_rows() %>%
    arrange(focal_ID, comp_ID) %>%
    mutate(growth_hat = NA) %>%
    select(
      # Relating to focal tree:
      focal_ID, focal_sp, dbh, foldID, geometry, growth,
      # Relating to competitor tree:
      comp_ID, dist, comp_sp, comp_basal_area
    )

  return(focal_vs_comp)
}



#' Fit Bayesian competition model
#'
#' @param focal_vs_comp data frame from \code{\link{create_focal_vs_comp}}
#' @param model_formula model formula of type competitor
#' @param run_shuffle boolean as to whether to run permutation test shuffle of
#'   competitor tree species within a particular focal_ID
#' @param prior_hyperparameters A list of `{a_0, b_0, mu_0, V_0}` prior hyperparameters
#'
#' @description Fit a Bayesian linear regression model with interactions terms where \deqn{y = X \beta + \epsilon}
#' \tabular{ll}{
#' \eqn{\mu} \tab mean hyperparameter vector for \eqn{\beta} of length \eqn{p + 1} \cr
#' \eqn{\V} \tab covariance hyperparameter matrix for \eqn{\beta} of dimension \eqn{(p + 1) \times (p + 1)} \cr
#' \eqn{a} \tab shape hyperparameter for \eqn{\sigma^2 > 0} \cr
#' \eqn{b} \tab scale hyperparameter for \eqn{\sigma^2 > 0}\cr
#' }
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#' @source Closed form solutions of Bayesian linear regression \url{https://doi.org/10.1371/journal.pone.0229930.s004}
#' @return A list of `{a_star, b_star, mu_star, V_star}` posterior hyperparameters
#' @export
#'
#' @examples
#' 1+1
fit_bayesian_model <- function(focal_vs_comp, model_formula, run_shuffle = FALSE,
                               prior_hyperparameters = NULL){
  if(FALSE){
    focal_vs_comp <- focal_vs_comp_bw
    model_formula <- model_formula_bw
    run_shuffle <- FALSE
    prior_hyperparameters <- NULL
  }

  # Prepare data for regression Generate data frame of all focal trees
  focal_trees <- focal_vs_comp %>%
    group_by(focal_ID, comp_sp) %>%
    # Sum basal area & count of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(
      # basal_area_total = sum(comp_basal_area),
      comp_basal_area = sum(comp_basal_area),
      # n_comp = n()
    )

  # Shuffle group label only if flag is set
  if(run_shuffle){
    focal_trees <- focal_trees %>%
      group_by(focal_ID) %>%
      mutate(comp_sp = sample(comp_sp))
  }

  # Continue processing focal_trees
  focal_trees <- focal_trees %>%
    # sum biomass and n_comp for competitors of same species. we need to do this
    # for the cases when we do permutation shuffle.
    group_by(focal_ID, comp_sp) %>%
    summarise_all(list(sum)) %>%
    # ungroup() %>%
    # compute biomass for each tree type
    # Note we have to specifically use spread() and not pivot_wider()
    # https://github.com/tidyverse/tidyr/issues/770 to use drop functionality
    spread(key = comp_sp, value = comp_basal_area, fill = 0, drop = FALSE) %>%
    group_by(focal_ID) %>%
    summarise_all(list(sum)) %>%
    ungroup()

  # Matrix and vector objects for analytic computation of all posteriors
  focal_trees <- focal_vs_comp %>%
    select(focal_ID, focal_sp, dbh, growth) %>%
    distinct() %>%
    left_join(focal_trees, by = "focal_ID") %>%
    rename(sp = focal_sp)

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
predict_bayesian_model <- function(focal_vs_comp, model_formula, posterior_param){

  if(FALSE){
    focal_vs_comp <- focal_vs_comp_bw
    # focal_vs_comp <- test
    model_formula <- model_formula_bw
    posterior_param <- bw_fit_model
  }

  # Prepare data for regression Generate data frame of all focal trees
  focal_trees <- focal_vs_comp %>%
    group_by(focal_ID, comp_sp) %>%
    # Sum basal area & count of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(
      # basal_area_total = sum(comp_basal_area),
      comp_basal_area = sum(comp_basal_area),
      # n_comp = n()
    )

  # Continue processing focal_trees
  focal_trees <- focal_trees %>%
    # sum biomass and n_comp for competitors of same species. we need to do this
    # for the cases when we do permutation shuffle.
    group_by(focal_ID, comp_sp) %>%
    summarise_all(list(sum)) %>%
    # ungroup() %>%
    # compute biomass for each tree type
    # Note we have to use spread and not pivot_wider https://github.com/tidyverse/tidyr/issues/770
    # pivot_wider(names_from = comp_sp, values_from = comp_basal_area, values_fill = 0) %>%
    spread(key = comp_sp, value = comp_basal_area, fill = 0, drop = FALSE) %>%
    group_by(focal_ID) %>%
    summarise_all(list(sum)) %>%
    ungroup()

  # # Might no longer need this
  # # Add biomass=0 for any species for which there are no trees
  # species_levels <- model_specs$species_of_interest
  # missing_species <- species_levels[!species_levels %in% names(focal_trees)] %>%
  #   as.character()
  # if(length(missing_species) > 0){
  #   for(i in 1:length(missing_species)){
  #     focal_trees <- focal_trees %>%
  #       mutate(!!missing_species[i] := 0)
  #   }
  #   focal_trees <- focal_trees %>%
  #     select(everything(), !!species_levels)
  # }

  # Matrix objects for analytic computation of all posteriors
  focal_trees <- focal_vs_comp %>%
    select(focal_ID, focal_sp, dbh, growth) %>%
    distinct() %>%
    left_join(focal_trees, by = "focal_ID") %>%
    rename(sp = focal_sp)

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



#' Run the bayesian model with spatial cross validation
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
run_cv <- function(focal_vs_comp, model_formula, max_dist, cv_grid,
                   run_shuffle = FALSE, prior_hyperparameters = NULL,
                   all_folds = TRUE){

  if(FALSE){
    # # Code to test SCBI
    # focal_vs_comp <- focal_vs_comp_scbi
    # model_specs <- scbi_specs
    # cv_grid <- scbi_cv_grid
    #
    # run_shuffle = FALSE
    # prior_hyperparameters = NULL
    # all_folds = FALSE


    # Code to test BigWoods
    focal_vs_comp <- focal_vs_comp_bw
    model_formula <- model_formula_bw
    cv_grid <- bw_cv_grid

    run_shuffle <- FALSE
    prior_hyperparameters <- NULL
    all_folds <- TRUE
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
  focal_trees <- vector(mode = "list", length = length(folds))

  for (i in 1:length(folds)){
    # first pull out the test set and the full train set
    train_full <- focal_vs_comp %>%
      filter(foldID != folds[i])
    test <- focal_vs_comp %>%
      filter(foldID == folds[i])

    if(nrow(test) == 0)
      next


    if(FALSE){
      # Visualize original folds
      cv_grid$plots

      # View training set (slow)
      train_full %>% sample_frac(0.01) %>% st_as_sf() %>% ggplot() + geom_sf()
    }

    # now buffer off the test fold by max_dist
    test_fold <- cv_grid$blocks %>%
      subset(folds == i)

    test_fold_boundary <- test_fold %>%
      st_bbox() %>%
      st_as_sfc()

    # Buffer extends out from test set boundary
    test_fold_boundary_buffer <- test_fold_boundary %>%
      st_buffer(dist = max_dist)

    train_fold_boolean <- train_full %>%
      st_as_sf() %>%
      st_intersects(test_fold_boundary_buffer, sparse = FALSE)

    train <- train_full %>%
      filter(!train_fold_boolean)

    if(FALSE){
      # Visualize original folds
      cv_grid$plots

      # Visualize test set trees + boundary
      ggplot() +
        geom_sf(data = test_fold_boundary_buffer, col = "red") +
        geom_sf(data = test_fold_boundary, col = "black") +
        geom_sf(data = train %>% sample_frac(0.01) %>% st_as_sf(), col = "blue", alpha = 0.1) +
        geom_sf(data = test %>% st_as_sf(), col = "red")
    }

    # now pretty easy to just call the two functions!
    fold_fit <- train %>%
      fit_bayesian_model(model_formula, run_shuffle = FALSE, prior_hyperparameters = NULL)


    focal_trees[[i]] <- test %>%
      predict_bayesian_model(model_formula, posterior_param = fold_fit)
  }

  focal_trees <- focal_trees %>%
    bind_rows()

  return(focal_trees)
}











#' Plot beta_0 parameters
#'
#' @inheritParams fit_bayesian_model
#' @param posterior_param Output of \code{\link{fit_bayesian_model}}
#'
#' @import ggridges
#' @importFrom mvnfast rmvt
#' @importFrom purrr set_names
#' @importFrom ggridges geom_density_ridges
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1+1
plot_beta0 <- function(posterior_param, species_list){
  if (FALSE){
    posterior_param <- bw_fit_model
    species_list <- c("evergreen", "maple", "misc", "oak", "short_tree", "shrub")
  }

  n_sim <- 1000
  nu_star <- 2*posterior_param$a_star
  Sigma_star <- (posterior_param$b_star/posterior_param$a_star)*posterior_param$V_star

  beta_lambda_posterior_df <-
    rmvt(n_sim, sigma = Sigma_star, mu = as.vector(posterior_param$mu_star), df = nu_star) %>%
    data.frame() %>%
    as_tibble() %>%
    set_names(colnames(posterior_param$V_star)) %>%
    gather(type, value)

  coefficient_types <- beta_lambda_posterior_df %>%
    select(type) %>%
    distinct() %>%
    mutate(
      coefficient_type =
        case_when(
          type == "(Intercept)" ~ "intercept",
          type %in% str_c("sp", species_list) ~ "intercept",
          str_detect(type, "dbh") ~ "dbh",
          type %in% species_list ~ "competition",
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

  baseline_species <- species_list[1] %>% as.character()

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
      type = ifelse(type == "(Intercept)", baseline_species, str_sub(type, nchar("sp")+1))
    ) %>%
    mutate(type = str_to_title(type)) %>%
    select(-offset)

  ggplot(posterior_beta_0, aes(x=value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlim(c(-0.1,0.5)) +
    labs(
      x = expression(paste(beta[0], " (cm ",y^{-1},')')),
      y = species_list
    )

}



#' Plot beta_0 parameters
#'
#' @inheritParams fit_bayesian_model
#' @param posterior_param Output of \code{\link{fit_bayesian_model}}
#'
#' @import ggridges
#' @importFrom mvnfast rmvt
#' @importFrom purrr set_names
#' @importFrom ggridges geom_density_ridges
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1+1
plot_posterior_parameters <- function(posterior_param, species_list){
  if (FALSE){
    posterior_param <- bw_fit_model
    species_list <- c("evergreen", "maple", "misc", "oak", "short_tree", "shrub")
  }

  plot_list <- NULL


  n_sim <- 1000
  nu_star <- 2*posterior_param$a_star
  Sigma_star <- (posterior_param$b_star/posterior_param$a_star)*posterior_param$V_star

  beta_lambda_posterior_df <-
    rmvt(n_sim, sigma = Sigma_star, mu = as.vector(posterior_param$mu_star), df = nu_star) %>%
    data.frame() %>%
    as_tibble() %>%
    set_names(colnames(posterior_param$V_star)) %>%
    gather(type, value)

  coefficient_types <- beta_lambda_posterior_df %>%
    select(type) %>%
    distinct() %>%
    mutate(
      coefficient_type =
        case_when(
          type == "(Intercept)" ~ "intercept",
          type %in% str_c("sp", species_list) ~ "intercept",
          str_detect(type, "dbh") ~ "dbh",
          type %in% species_list ~ "competition",
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

  baseline_species <- species_list[1] %>% as.character()




  # Intercept ------------------------------
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
      type = ifelse(type == "(Intercept)", baseline_species, str_sub(type, nchar("sp")+1))
    ) %>%
    mutate(type = str_to_title(type)) %>%
    select(-offset)

  plot_list[["beta_0"]] <- ggplot(posterior_beta_0, aes(x=value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlim(c(-0.1,0.5)) +
    labs(
      x = expression(paste(beta[0], " (cm ",y^{-1},')')),
      y = species_list
    )





  # DBH ------------------------------
  posterior_sample <- beta_lambda_posterior_df %>%
    filter(coefficient_type == "dbh")

  posterior_sample_baseline <- posterior_sample %>%
    filter(type == "dbh") %>%
    rename(offset = value) %>%
    select(sim_ID, offset)

  posterior_beta_dbh <- posterior_sample %>%
    left_join(posterior_sample_baseline, by = "sim_ID") %>%
    mutate(
      offset = ifelse(type == "dbh", 0, offset),
      value = value + offset,
      type = ifelse(type == "dbh", baseline_species, str_sub(type, nchar("sp")+1, -5))
    ) %>%
    mutate(type = str_to_title(type)) %>%
    select(-offset)

  plot_list[["beta_dbh"]] <-
    ggplot(posterior_beta_dbh, aes(x=value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlim(c(-0.05,0.15)) +
    labs(
      x = expression(paste(beta[DBH], " (",y^{-1},')')),
      y = 'Family'
    )





  # lambda ------------------------------
  posterior_sample <- beta_lambda_posterior_df %>%
    filter(coefficient_type == "competition")

  lambdas <- expand.grid(species_list, species_list) %>%
    as_tibble() %>%
    rename(competitor = Var1, focal = Var2) %>%
    mutate(
      lambda = str_c(competitor, focal, sep = "_on_"),
      values = list(NULL)
    )

  # effect of evergreen on oak focal tree:
  # evergreen + speciesoak:evergreen
  # which in reality is speciesmisc:evergreen + speciesoak:evergreen
  #
  # effect of evergreen on misc focal tree (which is baseline species):
  # evergreen
  # which in reality is speciesmisc:evergreen
  #
  # effect of misc on evergreen focal tree
  # misc + speciesevergreen:misc
  # which in reality is speciesmisc:misc + speciesevergreen:misc

  for(i in 1:nrow(lambdas)){
    competitor <- lambdas$competitor[i]
    focal <- lambdas$focal[i]

    if(focal == baseline_species){
      lambda_values <- posterior_sample %>%
        filter(type == competitor) %>%
        pull(value)
    } else {
      lambda_values <- posterior_sample %>%
        filter(type == competitor | type == str_c("sp", focal, ":", competitor, sep = "")) %>%
        select(sim_ID, type, value) %>%
        spread(type, value) %>%
        select(2:3) %>%
        rowSums()
    }
    lambdas$values[i] <- list(lambda_values)
  }


  lambdas <- lambdas %>%
    unnest(cols = c(values))

  # Also used in Fig S3 below
  spp_to_include <- species_list

  lambdas_subset <- lambdas %>%
    filter(competitor %in% spp_to_include,
           focal %in% spp_to_include) %>%
    mutate(competitor = str_to_title(competitor),
           focal = str_to_title(focal))

  plot_list[["lambdas"]] <- ggplot(lambdas_subset, aes(x = values, y = fct_rev(competitor))) +
    geom_density_ridges() +
    facet_wrap(~focal, ncol =length(spp_to_include)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlim(c(-0.6,0.6)) +
    theme(panel.spacing.x = unit(0,'cm')) +
    labs(title='Focal', y='Competitor', x = expression(lambda)) +
    scale_x_continuous(breaks=c(-0.5,0,0.5), labels=c('-0.5','0','0.5'))


  return(plot_list)
}
