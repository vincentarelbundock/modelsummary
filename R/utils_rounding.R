#' Convert numeric values to strings using the `sprintf` function. NA, NaN,
#' -Inf, and Inf are replaced by an empty string.
#'
#' @param x a numeric vector to be converted to string
#' @param fmt a character vector of format strings which will be fed to the
#' `sprintf` function. See ?sprintf for details.
#'
#' @return a rounded number as character
#' @noRd
rounding <- function(x, fmt = '%.3f', pval = FALSE, ...) {

    ## NA should return empty
    idx_na <- is.na(x)

    # p values
    if (is.numeric(fmt) && isTRUE(pval)) {
        pdigits <- -max(fmt, 3)
        x <- format.pval(
            x,
            digits = 1,
            nsmall = fmt,
            eps = 10^pdigits,
            scientific = FALSE)
    }

    # input: character, factor, logical
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

    # input: numeric
    } else {

        if (is.character(fmt)) {
            out <- sprintf(fmt, x, ...)

        } else if (is.numeric(fmt)) {
            # R >4.1.0 will make 0 invalid in format()
            if (fmt == 0) {
                out <- sprintf("%.0f", x)

            } else {
                # apply `format()` individually to each cell, otherwise if one
                # entry requires a lot of digits, then all the entries will be
                # padded to match.  scientific = 4 means that this converts to
                # scientific if the fixed version is 4 characters longer than
                # the scientific.
                fun <- function(x) {
                    format(x, digits = 1, nsmall = fmt, trim = TRUE, scientific = 4, ...)
                }
                out <- sapply(x, fun)
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
            if (settings_equal("format_numeric_latex", "siunitx") && !settings_equal("dcolumn_stars_mbox", TRUE)) {
                out <- sprintf("\\num{%s}", out)
            } else if (settings_equal("format_numeric_latex", c("dollars", "mathmode"))) {
                out <- sprintf("$%s$", out)
            }
        }
    }

    ## HTML: convert hyphen-minus to minus
    if (settings_equal("output_format", c("html", "kableExtra"))) {
        # in hebrew or chinese locales, the html minus signs does not appear and it underlines the whole number.
        # https://github.com/vincentarelbundock/modelsummary/issues/552
        if (settings_equal("modelsummary_format_numeric_html", "minus") && settings_equal("known_locale", TRUE)) {
            out <- gsub("\\-", "\u2212", out)
        } else if (settings_equal("modelsummary_format_numeric_html", c("mathjax", "dollars"))) {
            out <- sprintf("$%s$", out)
        }
    }
  }

  # NA should return empty
  out[idx_na] <- ""

  return(out)
}
