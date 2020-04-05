# This file includes convenience functions for `modelsummary`. These functions
# accomplish tasks we can already be done using functions from the `gt`
# package. They are only here for *convenience*, and not because they
# accomplish core functions of the `modelsummary` package. Since they are only
# convenience functions, they will not be exported to the package
# documentation. Moreover, they are only semi-supported. This means that the
# maintainer does not plan to improve those functions unless someone
# contributes a pull request on Github. Bug fixes may also take longer than for
# core functions.

#' A convenience function to render markdown to html in row and column labels
#' 
#' @param tab a `gt` table object
#' @param position character string determines wither row, column or both
#'   labels should be rendered.
#' @keywords internal
#' @note This function only works for HTML output, since the `gt` render tools
#' are less developed for LaTeX and RTF output.
fmt_labels_md <- function(tab, position = c('both', 'row', 'column')) {
    out <- tab
    if (match.arg(position) %in% c('both', 'row')) {
        out <- out %>% gt::fmt_markdown(columns = 1)
    }
    if (match.arg(position) %in% c('both', 'column')) {
        f <- function(x) stats::setNames(lapply(names(x$`_data`), gt::md), names(x$`_data`))
        out <- out %>% gt::cols_label(.list = f(.))
    }
    return(out)
}
