# This function ensures that fe_bayes_lr objects are constructed appropriately.
fe_bayes_lr_constr <- function(x) {
  check_inherits(x[["data"]], "data.frame")
  check_prior_params(x[["prior_params"]])
  check_post_params(x[["post_params"]])
}
