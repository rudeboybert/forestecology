#' Fit Bayesian competition model
#'
#' @param focal_vs_comp data frame from \code{\link{create_focal_vs_comp}}
#' @param run_shuffle boolean as to whether to run permutation test shuffle of
#'   competitor tree species within a particular focal_ID
#' @param prior_param A list of `{a_0, b_0, mu_0, V_0}` prior hyperparameters
#'
#' @description Fit a Bayesian linear regression model with interactions terms where \deqn{y = X \beta + \epsilon}
#' \tabular{ll}{
#' \eqn{\mu} \tab mean hyperparameter vector for \eqn{\beta} of length \eqn{p + 1} \cr
#' \eqn{V} \tab covariance hyperparameter matrix for \eqn{\beta} of dimension \eqn{(p + 1) x (p + 1)} \cr
#' \eqn{a} \tab shape hyperparameter for \eqn{\sigma^2 > 0} \cr
#' \eqn{b} \tab scale hyperparameter for \eqn{\sigma^2 > 0}\cr
#' }
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#' @source Closed-form solutions of Bayesian linear regression \url{https://doi.org/10.1371/journal.pone.0229930.s004}
#' @return A list of `{a_star, b_star, mu_star, V_star}` posterior hyperparameters
#' @seealso \code{\link{predict.fe_bayes_lm}}
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Load in focal versus comp
#' data(focal_vs_comp_ex)
#'
#' posterior_param_ex <- focal_vs_comp_ex %>%
#'   fe_bayes_lm(prior_param = NULL, run_shuffle = FALSE)
fe_bayes_lm <- function(focal_vs_comp, prior_param = NULL, run_shuffle = FALSE) {
  if (FALSE) {
    focal_vs_comp <- focal_vs_comp_bw
    run_shuffle <- FALSE
    prior_param <- NULL
  }

  # Create linear regression model formula object
  sp_list <- focal_vs_comp$focal_sp %>%
    levels() %>%
    sort()
  model_formula <- sp_list %>%
    paste(., "*sp", sep = "", collapse = " + ") %>%
    paste("growth ~ sp + dbh + dbh*sp + ", .) %>%
    as.formula()

  # Create matrices & vectors for Bayesian regression
  focal_trees <- focal_vs_comp %>%
    create_bayesian_model_data(run_shuffle = run_shuffle)

  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Set priors. If no prior_param specified:
  if (is.null(prior_param)) {
    # Prior parameters for sigma2:
    a_0 <- 250
    b_0 <- 25
    # Prior parameters for betas and lambdas:
    mu_0 <- rep(0, ncol(X)) %>%
      matrix(ncol = 1)
    V_0 <- ncol(X) %>% diag()
  } else {
    a_0 <- prior_param$a_0
    b_0 <- prior_param$b_0
    mu_0 <- prior_param$mu_0
    V_0 <- prior_param$V_0
  }

  # Compute posteriors
  # Posterior parameters for betas and lambdas:
  mu_star <- solve(solve(V_0) + t(X) %*% X) %*% (solve(V_0) %*% mu_0 + t(X) %*% y)
  V_star <- solve(solve(V_0) + t(X) %*% X)

  # Posterior parameters for sigma2
  a_star <- a_0 + n / 2
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
    V_star = V_star,
    sp_list = sp_list
  )

  structure(
    posterior_hyperparameters,
    class = c("fe_bayes_lm", class(posterior_hyperparameters))
  )
}





#' Make predictions based on fitted Bayesian model
#'
#' @description Applies fitted model from \code{\link{fe_bayes_lm}} and
#'   returns posterior predicted values.
#'
#' @param object Output of \code{\link{fe_bayes_lm}}: A list of
#'   `{a_star, b_star, mu_star, V_star}` posterior hyperparameters
#' @inheritParams fe_bayes_lm
#' @inheritParams create_focal_vs_comp
#' @param ... Currently igniored—only included for consistency with generic.
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr nest
#'
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @seealso \code{\link{fe_bayes_lm}}
#' @source Closed-form solutions of Bayesian linear regression \url{https://doi.org/10.1371/journal.pone.0229930.s004}
#' @export
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' library(ggplot2)
#'
#' # Load in posterior parameter example
#' # and growth data to compare to
#' data(posterior_param_ex, ex_growth_df)
#'
#' predictions <- posterior_param_ex %>%
#'   predict(focal_vs_comp = focal_vs_comp_ex) %>%
#'   right_join(ex_growth_df, by = c("focal_ID" = "ID"))
#'
#' predictions %>%
#'   ggplot(aes(growth, growth_hat)) +
#'   geom_point() +
#'   geom_abline(slope = 1, intercept = 0)
predict.fe_bayes_lm <- function(object, focal_vs_comp, ...) {
  if (FALSE) {
    focal_vs_comp <- focal_vs_comp_bw
    object <- bw_fit_model
  }

  # Create linear regression model formula object
  sp_list <- focal_vs_comp$focal_sp %>%
    levels() %>%
    sort()
  model_formula <- sp_list %>%
    paste(., "*sp", sep = "", collapse = " + ") %>%
    paste("growth ~ sp + dbh + dbh*sp + ", .) %>%
    as.formula()

  # Create matrices & vectors for Bayesian regression
  focal_trees <- focal_vs_comp %>%
    create_bayesian_model_data()
  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Make posterior predictions
  mu_star <- object$mu_star
  focal_trees <- focal_trees %>%
    mutate(growth_hat = as.vector(X %*% mu_star)) %>%
    select(focal_ID, growth_hat)

  # TODO: why do we return focal_vs_comp?? Shouldn't we return focal_trees (one
  # row per focal tree not per interaction)
  return(focal_trees)
}





#' Run the bayesian model with spatial cross validation
#'
#' @inheritParams fe_bayes_lm
#' @inheritParams create_focal_vs_comp
#' @param cv_grid \code{sf} polygon output from \code{\link[blockCV]{spatialBlock}}
#' @description Run cross-validation
#'
#' @import dplyr
#' @import sf
#' @import sfheaders
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#' @export
#'
#' @examples
#' 1 + 1
run_cv <- function(focal_vs_comp, max_dist, cv_grid, prior_param = NULL, run_shuffle = FALSE) {
  # For each fold, store resulting y-hat for each focal tree in list
  folds <- focal_vs_comp %>%
    pull(foldID) %>%
    unique() %>%
    sort()

  purrr::map_dfr(
    folds,
    fit_one_fold,
    focal_vs_comp,
    max_dist,
    cv_grid,
    prior_param,
    run_shuffle
  )
}

fit_one_fold <- function(fold, focal_vs_comp, max_dist,
                         cv_grid, prior_param, run_shuffle) {
  # Define test set and "full" training set (we will remove buffer region below)
  test <- focal_vs_comp %>%
    filter(foldID == fold)
  train_full <- focal_vs_comp %>%
    filter(foldID != fold)

  # If no trees in test skip, skip to next iteration in for loop
  if (nrow(test) == 0) {
    return(NULL)
  }

  # Define sf object of boundary of test fold
  test_fold_boundary <- cv_grid %>%
    subset(folds == fold) %>%
    st_bbox() %>%
    st_as_sfc()

  # Remove trees in training set that are part of test set and buffer region to test set
  train <- train_full %>%
    st_as_sf() %>%
    add_buffer_variable(direction = "out", size = max_dist, region = test_fold_boundary) %>%
    filter(buffer) %>%
    as_tibble()

  # Fit model on training data and predict on test
  posterior_param_fold <- train %>%
    fe_bayes_lm(prior_param = prior_param, run_shuffle = run_shuffle)

  test %>%
    predict(posterior_param = posterior_param_fold)
}





#' Create input data frame for Bayesian regression
#'
#' @inheritParams fe_bayes_lm
#'
#' @return Data frame that can be used for lm()
#' @description This function is used both by \code{\link{fe_bayes_lm}} and \code{\link{predict.fe_bayes_lm}}
#' @export
#' @examples
#' 1 + 1
create_bayesian_model_data <- function(focal_vs_comp, run_shuffle = FALSE) {
  # Prepare data for regression
  focal_trees <- focal_vs_comp %>%
    group_by(focal_ID, comp_sp) %>%
    # Sum basal area of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(comp_basal_area = sum(comp_basal_area))

  # Shuffle group label only if flag is set
  # TODO: Can we do this within a pipe, therefore we can connect above chain
  # with chain below, that way we can re-use this code for predict
  # below that doesn't use run_shuffle?
  if (run_shuffle) {
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

  return(focal_trees)
}
