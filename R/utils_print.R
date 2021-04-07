#' internal print method for raw html and latex
#'
#' @export
#' @keywords internal
#' @noRd
print.modelsummary_string <- function(x, ...) {
    cat(x, "\n", sep = "", ...)
    invisible(x)
}
