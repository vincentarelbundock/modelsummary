#' Make sure LaTeX and HTML are safe to compile
#'
#' @keywords internal
escape_string <- function(x) {
    if (settings_equal("escape", TRUE)) {
        if (settings_equal("output_format", c("html", "kableExtra"))) {
            out <- escape_html(x)
        } else if (settings_equal("output_format", c("latex", "latex_tabular"))) {
            out <- escape_latex(x)
        } else {
            out <- x
        }
    } else {
        out <- x
    }
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
    x <- gsub("&", "&amp;", x)
    x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x)
    x <- gsub("\"", "&quot;", x)
    x
}
