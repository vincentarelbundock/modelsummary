#' Convert numeric values to strings using the `sprintf` function. NA, NaN,
#' -Inf, and Inf are replaced by an empty string.
#'
#' @param x a numeric vector to be converted to string
#' @param fmt a character vector of format strings which will be fed to the
#' `sprintf` function. See ?sprintf for details.
#'
#' @return a rounded number as character
#' @noRd
rounding <- function(x, fmt = '%.3f', ...) {

  # character, factor, logical: do not round
  if (is.factor(x) || is.logical(x) || is.character(x)) {
    out <- as.character(x)

  # numeric
  } else {
    if (is.character(fmt)) {
      out <- sprintf(fmt, x, ...)
    } else if (is.numeric(fmt)) {
      # R >4.1.0 will make 0 invalid in format()
      if (fmt == 0) {
        out <- sprintf("%.0f", x)
      } else {
        out <- trimws(format(round(x, fmt), nsmall = fmt, ...))
      }
    } else if (is.function(fmt)) {
      out <- fmt(x)
    } else {
      out <- x
    }

    ## math: LaTeX with the `siunitx` package and \num{} function
    if (mssequal("output_format", c("latex", "latex_tabular"))) {
      out <- sprintf("\\num{%s}", out)
    ## math: HTML substitute minus sign
    } else if (mssequal("output_format", c("html", "kableExtra"))) {
      out <- gsub("-", "&minus;", out)
    }
  }


  out <- gsub('^NA$|^NaN$|^-Inf$|^Inf$', '', out)
  return(out)
}
