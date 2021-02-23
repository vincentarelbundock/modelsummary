#' sanity check
#'
#' @noRd
sanitize_vcov <- function(models, vcov, ...) {

  ellip <- list(...)

  # sanity_ellipsis must be called before sanity_vcov
  if ("statistic_override" %in% names(ellip)) {
    vcov <- ellip[["statistic_override"]]
  }

  if (is.null(vcov)) {
    return(NULL)
  }

  # default output
  out <- NULL

  # list of formulas, functions, matrices, or vectors
  # first class because some models inherit from "list"
  if (class(vcov)[1] == "list" & class(models)[1] == "list") {
    checkmate::assert_true(length(vcov) == length(models))
    for (s in vcov) {
      checkmate::assert(
        checkmate::check_formula(s),
        checkmate::check_function(s),
        checkmate::check_matrix(s),
        checkmate::check_vector(s),
        combine="or"
      )
    }
    out <- vcov
  }

  # single formulas/matrices/functions: apply to every model
  if (isTRUE(checkmate::check_formula(vcov)) ||
      isTRUE(checkmate::check_matrix(vcov))  ||
      isTRUE(checkmate::check_function(vcov))) {
    out <- rep(list(vcov), length(models))
  }

  if (is.character(vcov)) {
    checkmate::assert(
      checkmate::check_character(vcov, len=1),
      checkmate::check_character(vcov, len=length(models)))
    checkmate::assert_true(all(
      vcov %in% c("robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m",
                  "HC5", "stata", "classical", "constant", "iid")))
    if (length(vcov) == 1) {
      out <- as.list(rep(vcov, length(models)))
    } else {
      out <- as.list(vcov)
    }
  }

  if (is.null(out)) {
    stop("Please supply a valid input for the `vcov` argument. Read `?modelsummary`.") 
  }

  return(out)
}
