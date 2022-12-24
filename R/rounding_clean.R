rounding_clean <- function(x) {
    # Remove weird numbers before wrapping in siunitx
    out <- gsub("^NA$|^NaN$|^-Inf$|^Inf$", "", x)

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
    return(out)
}