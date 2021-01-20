#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot Bayesian model parameters
#'
#' @param object Output of \code{\link{fe_bayes_lm}}
#' @param type A single character string for plot type with possible values
#' "intercepts", "dbh_slopes", or "competition".
#' @param sp_to_plot Vector of subset of species to plot
#' @inheritParams predict.fe_bayes_lm
#'
#' @import ggridges
#' @importFrom mvnfast rmvt
#' @importFrom purrr set_names
#' @importFrom tidyr gather
#' @importFrom ggridges geom_density_ridges
#' @importFrom stats as.formula
#' @import stringr
#' @return \code{focal_vs_comp} with new column of predicted \code{growth_hat}
#'
#' @examples
#' library(ggplot2)
#' library(ggridges)
#'
#' # Load in posterior parameter example
#' data(posterior_param_ex)
#'
#' # Plot beta_0, growth intercepts
#' autoplot(posterior_param_ex, type = "intercepts")
#'
#' # Plot beta_dbh, growth-dbh slope
#' autoplot(posterior_param_ex, type = "dbh_slopes")
#'
#' # Plot lambdas, competition coefficients
#' autoplot(posterior_param_ex, type = "competition")
#' @export
autoplot.fe_bayes_lm <- function(object,
                                 type = "intercepts",
                                 sp_to_plot = NULL,
                                 ...) {

  # Identify all species and baseline category of species used for regression
  sp_list <- object$sp_list
  baseline_species <- sp_list %>%
    sort() %>%
    .[1]

  # Simulate observations from posterior
  beta_lambda_posterior_df <- simulate_beta_lambda_posterior(
    object,
    sp_list,
    baseline_species
  )

  switch(
    type,
    "intercepts" = intercepts_plot(
      beta_lambda_posterior_df,
      sp_to_plot,
      sp_list,
      baseline_species
    ),
    "dbh_slopes" = dbh_slopes_plot(
      beta_lambda_posterior_df,
      sp_to_plot,
      sp_list,
      baseline_species
    ),
    "competition" = competition_plot(
      beta_lambda_posterior_df,
      sp_to_plot,
      sp_list,
      baseline_species
    )
  )
}

simulate_beta_lambda_posterior <- function(posterior_param, sp_list,
                                           baseline_species) {
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
          # dbh_slopes for dbh
          str_detect(type, "dbh") ~ "dbh",
          # competition
          type %in% sp_list ~ "competition",
          str_detect(type, ":") ~ "competition",
          # Need this for everything else that aren't the two cases above:
          TRUE ~ "NA"
        )
    )

  beta_lambda_posterior_df %>%
    left_join(coefficient_types, by = "type") %>%
    select(type, coefficient_type, value) %>%
    group_by(type, coefficient_type) %>%
    mutate(sim_ID = 1:n()) %>%
    select(sim_ID, everything()) %>%
    ungroup()
}

# plot intercept coefficients
intercepts_plot <- function(beta_lambda_posterior_df, sp_to_plot,
                            sp_list, baseline_species) {
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

  ggplot(posterior_beta_0, aes(x = value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      x = expression(paste(beta[0], " (cm ", y^{
        -1
      }, ")")),
      y = "species"
    )
}

# plot dbh coefficients
dbh_slopes_plot <- function(beta_lambda_posterior_df, sp_to_plot,
                        sp_list, baseline_species) {
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

  ggplot(posterior_beta_dbh, aes(x = value, y = fct_rev(type))) +
    geom_density_ridges() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      x = expression(paste(beta[DBH], " (", y^{
        -1
      }, ")")),
      y = "species"
    )
}

# plot lambda coefficients
competition_plot <- function(beta_lambda_posterior_df, sp_to_plot,
                             sp_list, baseline_species) {
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
}
