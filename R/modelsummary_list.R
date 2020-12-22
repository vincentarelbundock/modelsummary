#' internal tidy method
#'
#' @export
#' @noRd
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
#' @noRd
glance.modelsummary_list <- function(x, ...) {
  if ("glance" %in% names(x)) {
    out <- x[["glance"]]
  } else {
    out <- data.frame(row.names=1)
  }
  out
}
