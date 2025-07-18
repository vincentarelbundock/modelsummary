#' Extract custom information from a model object and turn it into a tidy
#' data.frame or tibble
#'
#' To customize the output of a model of class `lm`, you can define a method
#' called `tidy_custom.lm` which returns a data.frame with a column called
#' "term", and the other columns you want to use as "estimate" or "statistic"
#' in your `modelsummary()` call. The output of this method must be similar to
#' the result of `tidy(model)`.
#'
#' @param x An object to be converted into a tidy data.frame or tibble.
#' @return A data.frame or tibble with information about model components.
#' @keywords internal
#' @export
tidy_custom <- function(x) {
  UseMethod("tidy_custom")
}

#' @inherit tidy_custom
#' @noRd
#' @export
tidy_custom.default <- function(x) NULL

#' Avoid namespace conflict when we want to customize glance internally and
#' still allow users to do the same with their own functions
#' @keywords internal
tidy_custom_internal <- function(x, ...) {
  UseMethod("tidy_custom_internal")
}

#' @inherit tidy_custom_internal
#' @keywords internal
#' @noRd
#' @export
tidy_custom_internal.default <- function(x, ...) NULL
