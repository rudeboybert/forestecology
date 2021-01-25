# Prompts
# ------------------------------------------------------------------------
glue_stop <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_warn <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::warn(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_message <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::inform(glue::glue(..., .sep = .sep, .envir = .envir))
}

# Check Utilities
# ------------------------------------------------------------------------
check_inherits <- function(x, what) {
  cl <- match.call()

  what <-
    switch(
      what,
      "numeric" = c("numeric", "integer"),
      "integer" = c("numeric", "integer"),
      "factor" = c("character", "factor"),
      "character" = c("character", "factor"),
      what
    )

  if (!inherits(x, what)) {
    glue_stop("Element `{list(cl$x)}` needs to inherit from `{list(what)}`, but its ",
              "class is `{list(class(x))}`.")
  }

  invisible(TRUE)
}

check_column <- function(column, type = NULL, df) {
  if (!column %in% colnames(df)) {
    glue_stop("The inputted data {deparse(substitute(df))} needs to contain ",
              'a "{column}" column, but it does not.')
  }

  if (!is.null(type)) {
    type <-
      switch(
        type,
        "numeric" = c("numeric", "integer"),
        "integer" = c("numeric", "integer"),
        "factor" = c("character", "factor"),
        "character" = c("character", "factor"),
        type
      )

    if (!inherits(df[[column]], type)) {
      glue_stop('The "{column}" column should inherit from {type}, but its ',
                "class is {list(class(df[[column]]))}.")
    }
  }

  invisible(TRUE)
}


# Argument Checking
# ------------------------------------------------------------------------
check_prior_params <- function(x) {
  check_inherits(x, "list")

  purrr::map2(
    c("a_0", "b_0", "mu_0", "V_0"),
    c("numeric", "numeric", "matrix", "matrix"),
    check_params_element,
    x,
    "prior"
  )

  invisible(TRUE)
}

check_post_params <- function(x) {
  check_inherits(x, "list")

  purrr::map2(
    c("a_star", "b_star", "mu_star", "V_star", "sp_list"),
    c("numeric", "numeric", "matrix", "matrix", "character"),
    check_params_element,
    x = x,
    params = "posterior"
  )

  invisible(TRUE)
}

check_params_element <- function(element, what, x, params) {
  if (!inherits(x[[element]], what)) {
    glue_stop("The {element} element of `{params}` needs to inherit from ",
              "class {what}, but its class is {list(class(x[[element]]))}.")
  }

  invisible(TRUE)
}

check_comp_bayes_lm <- function(comp_bayes_lm) {
  check_inherits(comp_bayes_lm, "list")
  check_prior_params(comp_bayes_lm$prior_params)
  check_post_params(comp_bayes_lm$post_params)
  check_inherits(comp_bayes_lm$terms, "formula")
}

check_focal_vs_comp <- function(focal_vs_comp) {
  check_inherits(focal_vs_comp, "tbl_df")

  purrr::map2(
    c("focal_ID", "focal_sp", "dbh", "foldID", "geometry", "growth", "comp"),
    c("numeric", "factor", "numeric", "factor", "sfc", "numeric", "list"),
    check_column,
    focal_vs_comp
  )
}
