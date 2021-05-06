#' sanity check
#'
#' @noRd
sanitize_vcov <- function(vcov, number_of_models, ...) {

  ellip <- list(...)

  # sanity_ellipsis must be called before sanity_vcov
  if ("statistic_override" %in% names(ellip)) {
    vcov <- ellip[["statistic_override"]]
  }

  if (is.null(vcov)) {
    # lengths must match to allow model recycling with multiple vcov
    out <- rep(list(NULL), number_of_models)
    return(out)
  }

  # default output
  out <- NULL

  # list of formulas, functions, matrices, or vectors
  # first class because some models inherit from "list"
  if (class(vcov)[1] == "list") {
    checkmate::assert(
      checkmate::check_true(length(vcov) == number_of_models),
      checkmate::check_true(number_of_models == 1))
    for (vcov_element in vcov) {
      checkmate::assert(
        checkmate::check_null(vcov_element),
        checkmate::check_formula(vcov_element),
        checkmate::check_function(vcov_element),
        checkmate::check_matrix(vcov_element),
        checkmate::check_numeric(vcov_element),
        checkmate::check_choice(vcov_element,
          choices = c("robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m",
                      "HC5", "stata", "classical", "constant", "iid", "HAC",
                      "NeweyWest", "bootstrap", "Andrews", "panel-corrected",
                      "outer-product", "weave")),
        checkmate::check_character(vcov_element, min.len = 2),
        combine = "or"
      )
    }
    out <- vcov
  }

  # single formulas/matrices/functions: apply to every model
  if (isTRUE(checkmate::check_formula(vcov)) ||
      isTRUE(checkmate::check_matrix(vcov))  ||
      isTRUE(checkmate::check_function(vcov))) {
    out <- rep(list(vcov), number_of_models)
  }

  if (is.character(vcov)) {
    checkmate::assert(
      checkmate::check_character(vcov, len = 1),
      checkmate::check_character(vcov, len = number_of_models),
      checkmate::check_true(number_of_models == 1))
    checkmate::assert_true(all(
      vcov %in% c("robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m",
                  "HC5", "stata", "classical", "constant", "iid", "HAC",
                  "NeweyWest", "bootstrap", "Andrews", "panel-corrected",
                  "outer-product", "weave")))
    if (length(vcov) == 1) {
      out <- as.list(rep(vcov, number_of_models))
    } else {
      out <- as.list(vcov)
    }
  }

  if (is.null(out)) {
    stop("Please supply a valid input for the `vcov` argument. Read `?modelsummary`.")
  }

  return(out)
}
