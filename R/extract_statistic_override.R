#' Internal method generic to extract statistic override
#'
#' @export
#' @keywords internal
override_statistic <- function(x, ...) {
  UseMethod("override_statistic")
}


#' Use the statistic_override vector to extract std.error/p.value/statistic
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibbl
#' @keywords internal
override_statistic.numeric <- function(x, statistic, ...) {
  # vector
  if (is.vector(x)) {
    out <- tibble::tibble(term = names(x), statistic = x)
  # matrix
  } else {
    out <- try(lmtest::coeftest(model, vcov=x), silent=TRUE)
    if (!inherits(out, "try-error")) {
      out <- generics::tidy(out)[, c("term", statistic)]
    } else {
      out <- x %>%
             base::diag() %>%
             sqrt %>%
             tibble::tibble(term = names(.), statistic = .)
    }
  }
  return(out)
}


#' Use the statistic_override vector to extract std.error/p.value/statistic
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibbl
#' @keywords internal
override_statistic.matrix <- override_statistic.numeric


#' Use the statistic_override vector to extract std.error/p.value/statistic
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibbl
#' @keywords internal
override_statistic.character <- function(x, ...) {
  checkmate::assert_atomic_vector(x)
  out <- tibble::tibble(term = names(x), statistic = x)
  return(out)
}


#' Use the statistic_override function to extract std.error
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibble
#' @keywords internal
override_statistic.function <- function(x, model, statistic, ...) {

  out <- try(lmtest::coeftest(model, vcov=x), silent=TRUE)

  if (inherits(out, "coeftest")) {
    out <- generics::tidy(out)[, c("term", statistic)]
  }

  if (inherits(out, "try-error")) {
    out <- try(x(model), silent=TRUE)
  }

  if (inherits(out, "matrix") || inherits(out, "numeric")) {
    out <- override_statistic.numeric(out, model=model, statistic=statistic) 
  }

  if (inherits(out, "character")) {
    out <- override_statistic.character(out, model=model, statistic=statistic)
  }

  if (inherits(out, "try-error")) {
    stop("Could not retrieve a valid variance-covariance matrix using the
         function supplied in `statistic_override`.")
  }

  return(out)
}


#' Allow users to override uncertainty estimates
#' @importFrom broom tidy
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
extract_statistic_override <- function(model, statistic_override,
                                       statistic="std.error") {

  checkmate::assert(
    checkmate::check_function(statistic_override),
    checkmate::check_matrix(statistic_override),
    checkmate::check_atomic_vector(statistic_override),
    combine="or")
  
  if (is.vector(statistic_override)) {
    checkmate::assert_true(all(names(statistic_override) == names(statistic_override)))
  }

  if (is.matrix(statistic_override)) {
    checkmate::assert_true(all(colnames(statistic_override) == names(statistic_override)))
  }

  out <- override_statistic(x=statistic_override, model=model,
                            statistic=statistic)
  colnames(out)[2] <- statistic

  return(out)

}
