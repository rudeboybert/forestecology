#' Create main data frame for analysis
#'
#' @inheritParams define_cv_grid
#' @inheritParams define_buffer
#' @param model_specs from \code{\link{get_model_specs}}
#' @return \code{focal_vs_comp} data frame
#' @export
#' @import dplyr
#' @importFrom proxy dist
#' @seealso \code{\link{define_cv_grid}} and \code{\link{get_model_specs}}
#' @examples
#' 1+1
create_focal_vs_comp <- function(growth_df, max_dist, species_notion, cv_grid, id){

  if(FALSE){
    growth_df <- bw_growth_df
    max_dist <- 7.5
    species_notion <- "sp"
    id <- "treeID"
    cv_grid <- bw_cv_grid
    i <- 1
  }

  # 1. Define focal trees where notion of "species" depends on
  # notion_of_focal_species
  growth_df_focal_trees <- growth_df %>%
    # Define notion of species as factor
    mutate(focal_notion_of_species = .data[[species_notion]] %>% factor()) %>%
    # Only trees alive both dates, not in buffer, and not a resprout at 2nd
    # census (OK to be resprout at first census):
    filter(dbh1 > 0, dbh2 > 0, !buffer, codes2 != 'R') %>%
    rename(dbh = dbh1) %>%
    # Assign species numerical code.
    # ID numbers to join focal trees with competitor trees
    mutate(focal_ID = .data[[id]]) %>%
    select(focal_ID, foldID, geometry, growth, focal_notion_of_species, dbh)

  # 2. Define competitor trees where notion of "species" depends on
  # notion_of_competitor_species
  growth_df_comp_trees <- growth_df %>%
    # Define notion of speices
    mutate(comp_notion_of_species = .data[[species_notion]] %>% factor()) %>%
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
  # species_of_interest == unique(bw$species)


  # Take a list approach instead of bind_rows(): code is 2x faster
  # https://r4ds.had.co.nz/iteration.html#unknown-output-length
  all_folds <- growth_df_focal_trees %>%
    pull(foldID) %>%
    unique() %>%
    sort()

  focal_vs_comp <- vector(mode = "list", length = length(all_folds))

  for(i in 1:length(all_folds)){
    # Identify focal and competitor trees in this fold
    growth_df_focal_trees_current_fold <- growth_df_focal_trees %>%
      filter(foldID == all_folds[i])

    # Narrow down focal trees to those max_dist away (towards inside) from boundary
    current_fold_boundary <- cv_grid$blocks %>%
      st_as_sf() %>%
      filter(folds == all_folds[i])


    # Clean this up:
    current_fold_competitor_boundary <- current_fold_boundary %>%
      compute_buffer_region(direction = "out", size = max_dist)
    comp_tree_index <- growth_df_comp_trees %>%
      st_intersects(current_fold_competitor_boundary, sparse = FALSE)
    growth_df_competitor_trees_current_fold <- growth_df_comp_trees %>%
      mutate(inside = as.vector(comp_tree_index)) %>%
      filter(inside)


    # Sanity check plot: for this fold, smaller black dots are competitor trees
    # and cyan larger dots are the test set. orange ones separating test set
    # from training set (trees in all other folds)
    if(FALSE){
      plot_title <- str_c("fold ", all_folds[i], ": Cyan dots = focal, orange dots = competitor")

      ggplot() +
        # geom_sf(data = bigwoods_study_region, col = "black") +
        geom_sf(data = current_fold_competitor_boundary, col = "red") +
        geom_sf(data = current_fold_boundary, col = "black") +
        geom_sf(data = growth_df_competitor_trees_current_fold, col = "orange", size = 3) +
        geom_sf(data = growth_df_focal_trees_current_fold, col = "cyan", size = 0.5) +
        labs(title = plot_title)
    }



    #
    # Convert this to a function! ----------------------------------------------
    #
    # Define data frame of distances
    distance_matrix <- growth_df_competitor_trees_current_fold %>%
      st_distance(growth_df_focal_trees_current_fold)
    focal_ID_current_fold <- growth_df_focal_trees_current_fold$focal_ID
    colnames(distance_matrix) <- focal_ID_current_fold
    comp_ID_current_fold <- growth_df_competitor_trees_current_fold$comp_ID
    rownames(distance_matrix) <- comp_ID_current_fold

    # All tidy, but difficult to understand
    # focal_vs_comp_current_fold <- distance_matrix %>%
    #   as_tibble(rownames = NA) %>%
    #   rownames_to_column(var = "comp_ID") %>%
    #   pivot_longer(cols = -comp_ID, names_to = "focal_ID", values_to = "dist") %>%
    #   mutate(focal_ID = as.numeric(focal_ID), comp_ID = as.numeric(comp_ID)) %>%
    #   arrange(focal_ID) %>%
    #   select(focal_ID, comp_ID, dist)

    focal_vs_comp_current_fold <-
      # Convert distance matrix to vector along with ID's
      tibble(
        focal_ID = rep(focal_ID_current_fold, each = length(comp_ID_current_fold)),
        comp_ID = rep(comp_ID_current_fold, times = length(focal_ID_current_fold)),
        dist = distance_matrix %>% as.vector()
      ) %>%
      # Remove cases where focal = comp
      filter(focal_ID != comp_ID)
    #
    # Convert this to a function! ----------------------------------------------
    #



    focal_vs_comp_current_fold <- focal_vs_comp_current_fold %>%
      # Remove pairs more than max_dist apart
      filter(dist < max_dist) %>%
      # Join focal tree data
      left_join(growth_df_focal_trees_current_fold, by = "focal_ID") %>%
      # Join competitor tree data
      left_join(growth_df_competitor_trees_current_fold, by = "comp_ID") %>%
      # Clean up mess from join:
      select(-c(foldID.y, geometry.y)) %>%
      rename(foldID = foldID.x, geometry = geometry.x)

    # Save current fold info
    focal_vs_comp[[i]] <- focal_vs_comp_current_fold
  }

  focal_vs_comp <- focal_vs_comp %>%
    # Convert list to tibble:
    bind_rows() %>%
    arrange(focal_ID, comp_ID) %>%
    mutate(growth_hat = NA) %>%
    select(
      # Relating to focal tree:
      focal_ID, focal_notion_of_species, dbh, foldID, geometry, growth,
      # Relating to competitor tree:
      comp_ID, dist, comp_notion_of_species, comp_basal_area
    )

  # Should we do grouping here?
  # group_by(focal_ID, focal_notion_of_species, dbh, foldID, geometry, growth) %>%
  # should we convert to sf object here?
  # st_as_sf()

  return(focal_vs_comp)
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
  focal_trees <- tibble(focal_ID = NA, growth_hat  = NA)

  for (i in folds){
    # first pull out the test set and the full train set
    train_full <- focal_vs_comp %>%
      filter(foldID != i)
    test <- focal_vs_comp %>%
      filter(foldID == i)

    if(FALSE){
      # Visualize original folds
      cv_grid$plots

      # View training set (slow)
      train %>% sample_frac(0.01) %>% st_as_sf() %>% ggplot() + geom_sf()
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
      fit_bayesian_model(model_specs, run_shuffle = run_shuffle)
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
#' @importFrom mvnfast rmvt
#' @importFrom purrr set_names

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
      set_names(colnames(posterior_param$V_star)) %>%
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





}

