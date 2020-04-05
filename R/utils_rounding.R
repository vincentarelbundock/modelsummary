#' Convert numeric values to strings using the `sprintf` function. NA, NaN,
#' -Inf, and Inf are replaced by an empty string.
#'
#' @param x a numeric vector to be converted to string
#' @param fmt a character vector of format strings which will be fed to the
#' `sprintf` function. See ?sprintf for details.
#'
#' @return a rounded number as character
#' @keywords internal
rounding <- function(x, fmt = '%.3f') {
    out <- sprintf(fmt, x)
    out <- stringr::str_replace(out, 'NA|NaN|-Inf|Inf', '')
    return(out)
}
