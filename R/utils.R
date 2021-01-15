# Checks and Prompts
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

check_inherits <- function(x, what) {
  cl <- match.call()

  if (!inherits(x, what)) {
    glue_stop("Element `{list(cl$x)}` needs to inherit from `{what}`, but its ",
              "class is `{list(class(x))}`.")
  }

  invisible(TRUE)
}

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
    x,
    "posterior"
  )

  invisible(TRUE)
}

check_params_element <- function(x, element, what, params) {
  if (!inherits(x[[element]], what)) {
    glue_stop("The {x[[element]]} element of `{params}` needs to inherit from ",
              "class {what}, but its class is {list(class(x[[element]]))}.")
  }

  invisible(TRUE)
}

# Re-exports
# ------------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit
