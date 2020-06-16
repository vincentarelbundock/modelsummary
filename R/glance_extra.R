#' Glance *more* at an object
#'
#' Construct a single row summary "glance" of a model, fit, or other object.
#' This method extracts more information than the usual `glance` method.
#' Defining a `glance_more` allows users to customize the features of a model
#' to display in a `modelsummary` table.
#'
#' glance_more methods must always return a one-row data frame 
#'
#' @param x model or other R object to convert to single-row data frame
#' @param ... other arguments passed to methods
#' @export
glance_more <- function(x, ...) {
  UseMethod("glance_more")
}



