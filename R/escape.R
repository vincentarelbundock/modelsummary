#' Make sure LaTeX and HTML are safe to compile
#'
#' @keywords internal
escape_string <- function(x, output_format = NULL) {
    if (is.null(output_format)) {
        output_format <- settings_get("output_format")
    }
    if (settings_equal("escape", TRUE)) {
        if (identical(Sys.getenv("pkgdown"),  "true") || output_format %in% c("html", "kableExtra")) {
            out <- escape_html(x)
        } else if (output_format %in% c("latex", "latex_tabular")) {
            out <- escape_latex(x)
        } else {
            out <- x
        }
    } else {
        out <- x
    }
    out = gsub("^`|`$", "", out)
    return(out)
}

#' Escape problematic characters to allow compilation in LaTeX
#'
#' Copied from `knitr` for internal use because it is unexported and CRAN
#' rejects :::
#'
#' @param x a character string to escape
#' @param newlines boolean
#' @param spaces boolean
#' @keywords internal
escape_latex <- function (x, newlines = FALSE, spaces = FALSE) {
    x <- gsub("\\\\", "\\\\textbackslash", x)
    x <- gsub("([#$%&_{}])", "\\\\\\1", x)
    x <- gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
    x <- gsub("~", "\\\\textasciitilde{}", x)
    x <- gsub("\\^", "\\\\textasciicircum{}", x)
    if (newlines)
        x <- gsub("(?<!\n)\n(?!\n)", "\\\\\\\\", x, perl <- TRUE)
    if (spaces) 
        x <- gsub("(?<<- ) ", "\\\\ ", x, perl <- TRUE)
    x
}

#' Escape problematic characters to allow display in HTML
#'
#' Copied from `knitr` for internal use because it is unexported and CRAN
#' rejects :::
#' @param x a character string to escape
#' @keywords internal
escape_html <- function (x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
}
