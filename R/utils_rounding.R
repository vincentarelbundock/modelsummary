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

  # character, factor, logical
  if (is.factor(x) || is.logical(x) || is.character(x)) {
    out <- as.character(x)

    ## escape
    if (settings_equal("escape", TRUE)) {
        out <- escape_string(out)
    }

    ## siunitx S-column: protect strings with {}
    if (settings_equal("output_format", c("latex", "latex_tabular")) &&
        settings_equal("siunitx_s", TRUE)) {
      out <- sprintf("{%s}", out)
    }

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

    ## LaTeX siunitx \num{}
    if (settings_equal("siunitx", TRUE) &&
        settings_equal("siunitx_num", TRUE) &&
        settings_equal("output_format", c("latex", "latex_tabular"))) {
      out <- sprintf("\\num{%s}", out)
    }

    ## HTML: convert hyphen-minus to minus
    if (settings_equal("output_format", c("html", "kableExtra"))) {
      out <- gsub("\\-", "\\−", out)
    }
  }

  out <- gsub('^NA$|^NaN$|^-Inf$|^Inf$', '', out)
  return(out)
}
