#' internal tidy method
#'
#' @export
#' @importFrom generics tidy
#' @keywords internal
tidy.modelsummary_list <- function(x, ...) {
  if ("tidy" %in% names(x)) {
    x[["tidy"]]
  } else {
    stop('The `modelsummary_list` list must include an element named "tidy".')
  }
}

#' internal glance method
#'
#' @export
#' @importFrom generics glance
#' @keywords internal
glance.modelsummary_list <- function(x, ...) {
  if ("glance" %in% names(x)) {
    out <- x[["glance"]]
  } else {
    out <- data.frame(row.names=1)
  }
  out
}
