#' sanity check
#'
#' @noRd
sanitize_vcov <- function(vcov, number_of_models, ...) {

  ellip <- list(...)

  # sanity_ellipsis must be called before sanity_vcov
  if ("statistic_override" %in% names(ellip)) {
    vcov <- ellip[["statistic_override"]]
  }

  # recycling vcov for multiple models
  # single string/formulas/matrices/functions: apply to every model
  if (is.null(vcov) ||
      isTRUE(checkmate::check_formula(vcov)) ||
      isTRUE(checkmate::check_matrix(vcov))  ||
      isTRUE(checkmate::check_function(vcov)) ||
      isTRUE(checkmate::check_numeric(vcov)) ||
      isTRUE(checkmate::check_character(vcov, len = 1))) {
    vcov <- rep(list(vcov), number_of_models)
  # character vector to list (can be any length for single model many vcov)
  } else if (isTRUE(checkmate::check_character(vcov))) {
    vcov <- as.list(vcov)
  # check first class because some models inherit from "list"
  } else if (class(vcov)[1] != "list") {
    stop("The value of the `vcov` argument is invalid. Please refer to the documentation.")
  }

  checkmate::check_true(length(vcov) == number_of_models)

  sandwich_types <- c("robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4",
                      "HC4m", "HC5", "stata", "classical", "constant", "iid",
                      "HAC", "NeweyWest", "bootstrap", "Andrews",
                      "panel-corrected", "outer-product", "weave")

  for (i in seq_along(vcov)) {
    checkmate::assert(
      checkmate::check_null(vcov[[i]]),
      checkmate::check_formula(vcov[[i]]),
      checkmate::check_function(vcov[[i]]),
      checkmate::check_matrix(vcov[[i]]),
      checkmate::check_numeric(vcov[[i]]),
      checkmate::check_choice(tolower(vcov[[i]]), choices = tolower(sandwich_types)),
      checkmate::check_character(vcov[[i]], min.len = 2),
      combine = "or"
    )

    # case-insensitive sandwich types
    if (isTRUE(checkmate::check_character(vcov[[i]], len = 1))) {
      vcov[[i]] <- sandwich_types[match(tolower(vcov[[i]]), tolower(sandwich_types))]
    }
  }

  return(vcov)
}
