#' Extract parameter estimates from a `modelsummary_list` object.
#'
#' A `modelsummary_list` is a simple list which holds two data.frames names
#' "glance" and "tidy", and to which we have attached the class
#' "modelsummary_list". This allows us to manually create objects that will be
#' printed nicely by `modelsummary` (see Examples section).
#'
#' @param x a list of class `modelsummary_list` (see Examples section)
#' @param ... other parameters are accepted by ignored
#' @examples
#' \dontrun{
#' mod <- list(
#'   tidy = data.frame(term = c("A", "B"),
#'                     estimate = 1:2,
#'                     std.error = 3:4),
#'   glance = data.frame(nobs = 10))
#' class(mod) <- c("modelsummary_list", class(mod))
#'
#' modelsummary(mod)
#' }
#' @noRd
#' @export
tidy.modelsummary_list <- function(x, ...) {
  if ("tidy" %in% names(x)) {
    x[["tidy"]]
  } else {
    stop('The `modelsummary_list` list must include an element named "tidy".')
  }
}

#' Extract goodness-of-fit statistics from a `modelsummary_list` object.
#'
#' @inherit tidy.modelsummary_list
#' @noRd
#' @export
glance.modelsummary_list <- function(x, ...) {
  if ("glance" %in% names(x)) {
    out <- x[["glance"]]
  } else {
    out <- data.frame(row.names = 1)
  }
  out
}
