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

    ## NA should return empty
    idx_na <- is.na(x)

    ## character, factor, logical
    if (is.factor(x) || is.logical(x) || is.character(x)) {
        out <- as.character(x)

        ## escape
        if (settings_equal("escape", TRUE)) {
            out <- escape_string(out)
        }

        ## siunitx S-column: protect strings with {}
        if (settings_equal("output_format", c("latex", "latex_tabular")) &&
            settings_equal("siunitx_scolumns", TRUE)) {
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


    # Remove weird numbers before wrapping in siunitx
    out <- gsub('^NA$|^NaN$|^-Inf$|^Inf$', '', out)

    ## LaTeX siunitx \num{}
    if (settings_equal("output_format", c("latex", "latex_tabular"))) {
        if (!isTRUE(settings_get("siunitx_scolumns"))) {
            if (settings_equal("format_numeric_latex", "siunitx")) {
                out <- sprintf("\\num{%s}", out)
            } else if (settings_equal("format_numeric_latex", c("dollars", "mathmode"))) {
                out <- sprintf("$%s$", out)
            }
        }
    }

    ## HTML: convert hyphen-minus to minus
    if (settings_equal("output_format", c("html", "kableExtra"))) {
        if (settings_equal("format_numeric_html", "minus")) {
            out <- gsub("\\-", "\u2212", out)
        } else if (settings_equal("format_numeric_html", c("mathjax", "dollars"))) {
            out <- sprintf("$%s$", out)
        }
    }
  }

  ## NA should return empty
  out[idx_na] <- ""

  return(out)
}
