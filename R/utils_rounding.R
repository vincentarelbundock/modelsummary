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
    if (!is.character(x)) {
        out <- sprintf(fmt, x)
    } else {
        out <- x
    }
    out <- gsub('^NA$|^NaN$|^-Inf$|^Inf$', '', out) 
    return(out)
}
