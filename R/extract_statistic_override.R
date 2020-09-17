#' Allow users to override uncertainty estimates
#' @importFrom broom tidy
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
extract_statistic_override <- function(model, statistic_override, conf_level=NULL) {

  # needed for logic tests
  out <- NULL

  # sanity checks at the individual level
  checkmate::assert(
    checkmate::check_function(statistic_override),
    checkmate::check_matrix(statistic_override),
    checkmate::check_atomic_vector(statistic_override, names="named"),
    combine = "or")

  # lmtest::coeftest
  if (is.function(statistic_override) || is.matrix(statistic_override)) {
    out <- try(lmtest::coeftest(model, statistic_override), silent=TRUE)

    # if coeftest works, use tidy and return data.frame
    if (!inherits(out, "try-error")) {
      if (!is.null(conf_level)) {
        out <- generics::tidy(out, conf.int=TRUE, conf.level=conf_level)
      } else {
        out <- generics::tidy(out)
      }
    }
    
    if (inherits(out, "data.frame")) {
      return(out)
    }

  }

  # function is expected to return a matrix
  if (is.function(statistic_override)) {
    out <- try(statistic_override(model), silent=TRUE)
  }

  if (is.matrix(out)) {
    out <- base::diag(out) %>%
      sqrt %>%
      data.frame(term=names(.), std.error=.)
    return(out)
  } 

  # atomic vector
  if (is.atomic(statistic_override)) {
    out <- data.frame(term=names(statistic_override), std.error=statistic_override)
    return(out)
  }

  stop("Could not retrieve a valid variance-covariance matrix using the function supplied in `statistic_override`.")

}
