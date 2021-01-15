# fe_data Class
# ------------------------------------------------------------------------



# fe_bayes_lr Class
# ------------------------------------------------------------------------
init_fe_bayes_lr <- function(fe_data, prior_params, post_params) {
  out <-
    list(
      fe_data = fe_data,
      prior_params = prior_params,
      post_params = post_params
    )

  structure(
    out,
    class = c("fe_bayes_lr", class(out))
  )
}

constr_fe_bayes_lr <- function(x) {
  check_inherits(x[["fe_data"]], "data.frame")
  check_prior_params(x[["prior_params"]])
  check_post_params(x[["post_params"]])

  x
}
