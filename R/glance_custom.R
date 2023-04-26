#' Extract custom information from a model object and turn it into a tidy
#' data.frame or tibble with a single row.
#'
#' To customize the output of a model of class `lm`, you can define a new
#' method called `glance_custom.lm` which returns a one-row data.frame.
#'
#' @param x model or other R object to convert to single-row data frame
#' @param ... ellipsis
#'
#' @keywords internal
#' @export
glance_custom <- function(x, ...) {
  UseMethod("glance_custom")
}

#' @inherit glance_custom
#' @noRd
#' @export
glance_custom.default <- function(x, ...) NULL


#' Avoid namespace conflict when we want to customize glance internally and
#' still allow users to do the same with their own functions
#' @keywords internal
glance_custom_internal <- function(x, ...) {
  UseMethod("glance_custom_internal")
}

#' @inherit glance_custom_internal
#' @keywords internal
glance_custom_internal.default <- function(x, ...) NULL
