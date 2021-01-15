#' Fit Bayesian competition model
#'
#' @param object data frame from \code{\link{create_focal_vs_comp}}
#' @param run_shuffle boolean as to whether to run permutation test shuffle of
#'   competitor tree species within a particular focal_ID
#' @param prior_param A list of `{a_0, b_0, mu_0, V_0}` prior hyperparameters
#' @param ... Currently ignored, only included for consistency with generic.
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
#' @seealso [predict.fe_bayes_lr]
#'
#' @method fit fe_data
#' @export fit.fe_data
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Load in focal versus comp
#' data(focal_vs_comp_ex)
#'
#' posterior_param_ex <- focal_vs_comp_ex %>%
#'   fit(prior_param = NULL, run_shuffle = FALSE)
fit.fe_data <- function(object, prior_param = NULL, run_shuffle = FALSE, ...) {
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

  # Append posterior parameters
  object[["post_params"]] <- list(
    a_star = a_star,
    b_star = b_star,
    mu_star = mu_star,
    V_star = V_star,
    sp_list = sp_list
  )

  object
}





# For now, we haven't solidified what `fe_data` objects should look like.
# Since they subclass `tbl_df`, we'll define the fit method for them.
#' @rdname fit.fe_data
#' @method fit tbl_df
#' @export fit.tbl_df
#' @export
fit.tbl_df <- fit.fe_data





#' Make predictions based on fitted Bayesian model
#'
#' @description Applies fitted model from [fit.fe_data] and
#'   returns posterior predicted values.
#'
#' @inheritParams fit.fe_data
#' @param posterior_param Output of [fit.fe_data]: A fitted `fe_bayes_lr` object.
#' @inheritParams create_focal_vs_comp
#'
#' @import dplyr
#' @importFrom stats model.matrix
#' @importFrom tidyr nest
#' @return A tibble with column `.pred`
#' @seealso [fit.fe_data]
#' @source Closed-form solutions of Bayesian linear regression \url{https://doi.org/10.1371/journal.pone.0229930.s004}
#' @importFrom stats predict
#' @method predict fe_bayes_lr
#' @export predict.fe_bayes_lr
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
#' predictions <- focal_vs_comp_ex %>%
#'   predict(posterior_param = posterior_param_ex) %>%
#'   right_join(ex_growth_df, by = c("focal_ID" = "ID"))
#' predictions %>%
#'   ggplot(aes(growth, .pred)) +
#'   geom_point() +
#'   geom_abline(slope = 1, intercept = 0)
predict.fe_bayes_lr <- function(object, posterior_param, ...) {
  if (FALSE) {
    focal_vs_comp <- focal_vs_comp_bw
    posterior_param <- bw_fit_model
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
  mu_star <- posterior_param$mu_star

  focal_trees %>%
    transmute(.pred = as.vector(X %*% mu_star))
}





#' Run the bayesian model with spatial cross validation
#'
#' @inheritParams fit.fe_data
#' @inheritParams create_focal_vs_comp
#' @param cv_grid \code{sf} polygon output from \code{\link[blockCV]{spatialBlock}}
#' @description Run cross-validation
#'
#' @import dplyr
#' @import sf
#' @import sfheaders
#' @return \code{focal_vs_comp} with new column of predicted \code{.pred}
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
  focal_trees <- vector(mode = "list", length = length(folds))

  for (i in 1:length(folds)) {
    # Define test set and "full" training set (we will remove buffer region below)
    test <- focal_vs_comp %>%
      filter(foldID == folds[i])
    train_full <- focal_vs_comp %>%
      filter(foldID != folds[i])

    # If no trees in test skip, skip to next iteration in for loop
    if (nrow(test) == 0) {
      next
    }

    # Define sf object of boundary of test fold
    test_fold_boundary <- cv_grid %>%
      subset(folds == i) %>%
      st_bbox() %>%
      st_as_sfc()

    # Remove trees in training set that are part of test set and buffer region to test set
    train <- train_full %>%
      st_as_sf() %>%
      add_buffer_variable(direction = "out", size = max_dist, region = test_fold_boundary) %>%
      filter(buffer) %>%
      as_tibble()

    if (FALSE) {
      # Visualize test set trees + boundary
      ggplot() +
        geom_sf(data = train %>% sample_frac(0.01) %>% st_as_sf(), col = "black", alpha = 0.1) +
        geom_sf(data = test_fold_boundary, col = "orange", fill = "transparent") +
        geom_sf(data = test %>% st_as_sf(), col = "orange")
    }

    # Fit model on training data and predict on test
    posterior_param_fold <- train %>%
      fit(prior_param = prior_param, run_shuffle = run_shuffle)

    focal_trees[[i]] <- test %>%
      predict(posterior_param = posterior_param_fold)
  }

  # Convert list to data frame and return
  focal_trees <- focal_trees %>%
    bind_rows()
  return(focal_trees)
}






#' Create input data frame for Bayesian regression
#'
#' @inheritParams fit.fe_data
#'
#' @return Data frame that can be used for lm()
#' @description This function is used both by [fit.fe_data] and [predict.fe_bayes_lr]
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





#' Plot Bayesian model parameters
#'
#' @param posterior_param Output of \code{\link{fit.fe_data}}
#' @param sp_to_plot Vector of subset of species to plot
#'
#' @import ggridges
#' @importFrom mvnfast rmvt
#' @importFrom purrr set_names
#' @importFrom tidyr gather
#' @importFrom ggridges geom_density_ridges
#' @importFrom stats as.formula
#' @import stringr
#' @return \code{focal_vs_comp} with new column of predicted \code{.pred}
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggridges)
#' # Load in posterior parameter example
#' data(posterior_param_ex)
#' plots <- plot_bayesian_model_parameters(posterior_param_ex)
#'
#' # Plot beta_0, growth intercepts
#' plots[[1]]
#'
#' # Plot beta_dbh, growth-dbh slopt
#' plots[[2]]
#'
#' # Plot lambdas, competition coefficents
#' plots[[3]]
plot_bayesian_model_parameters <- function(posterior_param, sp_to_plot = NULL) {

  # Identify all species and baseline category of species used for regression
  sp_list <- posterior_param$sp_list
  baseline_species <- sp_list %>%
    sort() %>%
    .[1]

  # 1. Simulate observations from posterior ------------------------------------
  n_sim <- 1000
  nu_star <- 2 * posterior_param$a_star
  Sigma_star <- (posterior_param$b_star / posterior_param$a_star) * posterior_param$V_star

  beta_lambda_posterior_df <-
    rmvt(n_sim, sigma = Sigma_star, mu = as.vector(posterior_param$mu_star), df = nu_star) %>%
    data.frame() %>%
    as_tibble() %>%
    set_names(colnames(posterior_param$V_star)) %>%
    gather(key = type, value = value)

  # Coefficient names in beta_lambda_posterior_df are a little hard to work with,
  # so clean them up and join to beta_lambda_posterior_df
  coefficient_types <- beta_lambda_posterior_df %>%
    select(type) %>%
    distinct() %>%
    mutate(
      coefficient_type =
        case_when(
          # intercepts:
          type == "(Intercept)" ~ "intercept",
          type %in% str_c("sp", sp_list) ~ "intercept",
          # slopes for dbh
          str_detect(type, "dbh") ~ "dbh",
          # competition
          type %in% sp_list ~ "competition",
          str_detect(type, ":") ~ "competition",
          # Need this for everything else that aren't the two cases above:
          TRUE ~ "NA"
        )
    )

  beta_lambda_posterior_df <- beta_lambda_posterior_df %>%
    left_join(coefficient_types, by = "type") %>%
    select(type, coefficient_type, value) %>%
    group_by(type, coefficient_type) %>%
    mutate(sim_ID = 1:n()) %>%
    select(sim_ID, everything()) %>%
    ungroup()

  # Save plots here
  plot_list <- vector(mode = "list", length = 3)


  # 2. Plot intercept coefficients ------------------------------
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
      type = ifelse(type == "(Intercept)", baseline_species, str_sub(type, nchar("sp") + 1))
    ) %>%
    # mutate(type = str_to_title(type)) %>%
    select(-offset)

  # When we only want to plot a subset of species:
  if (!is.null(sp_to_plot)) {
    sp_to_plot <- sort(sp_to_plot)

    posterior_beta_0 <- posterior_beta_0 %>%
      filter(type %in% sp_to_plot)
  }


  plot_list[["beta_0"]] <-
    ggplot(posterior_beta_0, aes(x = value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      x = expression(paste(beta[0], " (cm ", y^{
        -1
      }, ")")),
      y = "species"
    )


  # 3. Plot dbh coefficients ------------------------------
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
      type = ifelse(type == "dbh", baseline_species, str_sub(type, nchar("sp") + 1, -5))
    ) %>%
    # mutate(type = str_to_title(type)) %>%
    select(-offset)

  # When we only want to plot a subset of species:
  if (!is.null(sp_to_plot)) {
    sp_to_plot <- sort(sp_to_plot)

    posterior_beta_dbh <- posterior_beta_dbh %>%
      filter(type %in% sp_to_plot)
  }

  plot_list[["beta_dbh"]] <-
    ggplot(posterior_beta_dbh, aes(x = value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      x = expression(paste(beta[DBH], " (", y^{
        -1
      }, ")")),
      y = "species"
    )



  # 4. Plot lambda coefficients ------------------------------
  posterior_sample <- beta_lambda_posterior_df %>%
    filter(coefficient_type == "competition")

  posterior_lambda <- expand.grid(sp_list, sp_list) %>%
    as_tibble() %>%
    rename(competitor = Var1, focal = Var2) %>%
    mutate(
      lambda = str_c(competitor, focal, sep = "_on_"),
      values = list(NULL)
    )

  # We need to add baseline offset to all categories.
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

  for (i in 1:nrow(posterior_lambda)) {
    competitor <- posterior_lambda$competitor[i]
    focal <- posterior_lambda$focal[i]

    if (focal == baseline_species) {
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
    posterior_lambda$values[i] <- list(lambda_values)
  }

  posterior_lambda <- posterior_lambda %>%
    unnest(cols = c(values))

  # When we only want to plot a subset of species:
  if (!is.null(sp_to_plot)) {
    sp_to_plot <- sort(sp_to_plot)

    posterior_lambda <- posterior_lambda %>%
      filter(
        competitor %in% sp_to_plot,
        focal %in% sp_to_plot
      )
  }

  n_sp_to_plot <- posterior_lambda %>%
    pull(competitor) %>%
    unique() %>%
    length()

  plot_list[["lambda"]] <-
    ggplot(posterior_lambda, aes(x = values, y = fct_rev(competitor))) +
    geom_density_ridges() +
    facet_wrap(~focal, ncol = n_sp_to_plot) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    # theme(panel.spacing.x = unit(0, "cm")) +
    labs(
      x = expression(lambda),
      y = "Species",
      title = "Competitor species in rows, focal species in columns",
      subtitle = str_c("Ex: Top row, second column = competitive effect of", sp_to_plot[1], "on", sp_to_plot[2], sep = " ")
    )

  return(plot_list)
}
