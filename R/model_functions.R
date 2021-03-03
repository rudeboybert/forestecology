#' Fit Bayesian competition model
#'
#' @description Fit a Bayesian linear regression model with interactions terms where \deqn{y = X \beta + \epsilon}
#' \tabular{ll}{
#' \eqn{\mu} \tab mean hyperparameter vector for \eqn{\beta} of length \eqn{p + 1} \cr
#' \eqn{V} \tab covariance hyperparameter matrix for \eqn{\beta} of dimension \eqn{(p + 1) x (p + 1)} \cr
#' \eqn{a} \tab shape hyperparameter for \eqn{\sigma^2 > 0} \cr
#' \eqn{b} \tab scale hyperparameter for \eqn{\sigma^2 > 0}\cr
#' }
#'
#' @param focal_vs_comp data frame from [create_focal_vs_comp()]
#' @param run_shuffle boolean as to whether to run permutation test shuffle of
#'   competitor tree species within a particular focal_ID
#' @param prior_param A list of `{a_0, b_0, mu_0, V_0}` prior hyperparameters.
#'   Defaults to `a_0 = 250`, `b_0 = 250`, `mu_0` a vector of zeros of
#'   length \eqn{p + 1}, `V_0` an identity matrix of dimension \eqn{(p + 1) x (p
#'   + 1)}
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#'
#' @source Closed-form solutions of Bayesian linear
#' regression \url{https://doi.org/10.1371/journal.pone.0229930.s004}
#'
#' @return A list of `{a_star, b_star, mu_star, V_star}` posterior hyperparameters
#'
#' @family modeling functions
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Load in focal versus comp
#' data(focal_vs_comp_ex)
#'
#' comp_bayes_lm_ex <- focal_vs_comp_ex %>%
#'   comp_bayes_lm(prior_param = NULL, run_shuffle = FALSE)
comp_bayes_lm <- function(focal_vs_comp, prior_param = NULL, run_shuffle = FALSE) {
  check_focal_vs_comp(focal_vs_comp)
  if (!is.null(prior_param)) {
    check_prior_params(prior_param)
  }
  check_inherits(run_shuffle, "logical")

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
    create_bayes_lm_data(run_shuffle = run_shuffle)

  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Set priors. If no prior_param specified:
  if (is.null(prior_param)) {
    prior_param <- default_prior_params(X)
  }

  a_0 <- prior_param$a_0
  b_0 <- prior_param$b_0
  mu_0 <- prior_param$mu_0
  V_0 <- prior_param$V_0

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
  post_params <- list(
    a_star = a_star,
    b_star = b_star,
    mu_star = mu_star,
    V_star = V_star,
    sp_list = sp_list
  )

  out <- list(
    prior_params = prior_param,
    post_params = post_params,
    terms = model_formula
  )

  structure(
    out,
    class = c("comp_bayes_lm", class(out))
  )
}





default_prior_params <- function(X) {
  list(
    # Prior parameters for sigma2:
    a_0 = 250,
    b_0 = 25,
    # Prior parameters for betas and lambdas:
    mu_0 = rep(0, ncol(X)) %>%
      matrix(ncol = 1),
    V_0 = ncol(X) %>% diag()
  )
}



#' @importFrom stringr str_replace_all
#' @export
print.comp_bayes_lm <- function(x, ...) {
  cat(
    paste0(
      "Bayesian linear regression model parameters with a multivariate Normal likelihood. See ?comp_bayes_lm for details:\n\n"
    )
  )

  term_tbl <-
    utils::capture.output(
      print(
        tibble(
          parameter_type = c(rep("Inverse-Gamma on sigma^2", 2), rep("Multivariate t on beta", 2)),
          prior = c("a_0", "b_0", "mu_0", "V_0"),
          posterior = c("a_star", "b_star", "mu_star", "V_star")
        )
      )
    )
  cat(term_tbl[c(2, 4:length(term_tbl))], sep = "\n")

  cat(
    paste0(
      "\n",
      "Model formula:\n",
      x[[3]][2] %>% as.character(), " ~ ", x[[3]][3] %>% as.character() %>% str_replace_all("\n    ", ""),
      "\n"
    )
  )

  # # a and b parameters
  # term_tbl <-
  #   utils::capture.output(
  #     print(
  #       tibble(
  #         term = c("a", "b"),
  #         prior = unlist(unname(x$prior_params[1:2])),
  #         posterior = unlist(unname(x$post_params[1:2]))
  #       )
  #     )
  #   )
  # cat(term_tbl[2:length(term_tbl)], sep = "\n\n")
  #
  # # mu vector parameter
  # term_tbl <-
  #   utils::capture.output(
  #     print(
  #       tibble(
  #         prior = unlist(unname(x$prior_params[3])),
  #         posterior = unlist(unname(x$post_params[3]))
  #       )
  #     )
  #   )
  # cat(term_tbl[2:length(term_tbl)], sep = "\n\n")
  #
  # # V matrix ???
}





#' Make predictions based on fitted Bayesian model
#'
#' @description Applies fitted model from [comp_bayes_lm()] and
#'   returns posterior predicted values.
#'
#' @param object Output of [comp_bayes_lm()]: A list of
#'   `{a_star, b_star, mu_star, V_star}` posterior hyperparameters
#' @param newdata A data frame of type `focal_vs_comp` in which to look for variables with which to predict.
#' @param ... Currently ignored—only included for consistency with generic.
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr nest
#'
#' @return A vector of predictions with length equal to the input data.
#'
#' @family modeling functions
#'
#' @source Closed-form solutions of Bayesian linear
#' regression \url{https://doi.org/10.1371/journal.pone.0229930.s004}
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' library(ggplot2)
#'
#' # Load in posterior parameter example
#' # and growth data to compare to
#' data(comp_bayes_lm_ex, growth_ex)
#'
#' predictions <- focal_vs_comp_ex %>%
#'   mutate(growth_hat = predict(comp_bayes_lm_ex, focal_vs_comp_ex))
#'
#' predictions %>%
#'   ggplot(aes(growth, growth_hat)) +
#'   geom_point() +
#'   geom_abline(slope = 1, intercept = 0)
predict.comp_bayes_lm <- function(object, newdata, ...) {
  check_comp_bayes_lm(object)
  check_focal_vs_comp(newdata)

  # Create linear regression model formula object
  model_formula <- object$terms

  # Create matrices & vectors for Bayesian regression
  focal_trees <- newdata %>%
    create_bayes_lm_data()
  X <- model.matrix(model_formula, data = focal_trees)
  y <- focal_trees %>%
    pull(growth) %>%
    matrix(ncol = 1)
  n <- nrow(X)

  # Make posterior predictions
  mu_star <- object$post_params$mu_star
  as.vector(X %*% mu_star)
}





#' Run the bayesian model with spatial cross validation
#'
#' @description
#' This function carries out the bayesian modeling process with spatial
#' cross-validation as described in Allen and Kim (2020). Given a
#' focal-competitor data frame, it appends a column with predicted growth values.
#'
#' @inheritParams comp_bayes_lm
#' @inheritParams create_focal_vs_comp
#' @param cv_grid \code{sf} polygon output from \code{\link[blockCV]{spatialBlock}}
#'
#' @import dplyr
#' @import sf
#' @import sfheaders
#' @importFrom tibble enframe
#'
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#'
#' @family modeling functions
#'
#' @export
#'
#' @examples
#'
#' run_cv(
#'   focal_vs_comp_ex,
#'   comp_dist = 1,
#'   cv_grid = blocks_ex
#' )
run_cv <- function(focal_vs_comp, comp_dist, cv_grid, prior_param = NULL, run_shuffle = FALSE) {
  check_focal_vs_comp(focal_vs_comp)
  check_inherits(comp_dist, "numeric")
  check_inherits(cv_grid, "sf")
  if (!is.null(prior_param)) {
    check_prior_params(prior_param)
  }
  check_inherits(run_shuffle, "logical")

  # For each fold, store resulting y-hat for each focal tree in list
  folds <- focal_vs_comp %>%
    pull(foldID) %>%
    unique() %>%
    sort()

  purrr::map_dfr(
    folds,
    fit_one_fold,
    focal_vs_comp,
    comp_dist,
    cv_grid,
    prior_param,
    run_shuffle
  )
}

fit_one_fold <- function(fold, focal_vs_comp, comp_dist,
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
    add_buffer_variable(direction = "out", size = comp_dist, region = test_fold_boundary) %>%
    filter(buffer) %>%
    as_tibble()

  # Fit model on training data
  comp_bayes_lm_fold <- train %>%
    comp_bayes_lm(prior_param = prior_param, run_shuffle = run_shuffle)

  # Compute predicted values and append to test
  test %>%
    mutate(growth_hat = predict(comp_bayes_lm_fold, test))
}





#' Create input data frame for Bayesian regression
#'
#' @description
#' This function "widens" focal-competitor data frames for use inside of
#' package modeling functions, where each `comp_sp` inside of the `comp`
#' list-column receives its own column with its associated total basal area.
#'
#' This function is used internally by [comp_bayes_lm()] and
#' [predict.comp_bayes_lm()] exported as a convenience for
#' applications extending this package's functionality.
#'
#' @inheritParams comp_bayes_lm
#'
#' @importFrom tidyr unnest
#' @export
#'
#' @return Data frame for internal package use.
#'
#' @family modeling functions
#' @family data processing functions
#'
#' @examples
#' create_bayes_lm_data(focal_vs_comp_ex)
create_bayes_lm_data <- function(focal_vs_comp, run_shuffle = FALSE) {
  # Prepare data for regression
  focal_trees <- focal_vs_comp %>%
    unnest(comp) %>%
    group_by(focal_ID, comp_sp) %>%
    # Sum basal area of all neighbors; set to 0 for cases of no neighbors
    # within range.
    summarise(comp_basal_area = sum(comp_basal_area))

  # Shuffle group label only if flag is set
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
