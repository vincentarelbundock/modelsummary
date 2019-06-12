#' Utility function to cleanup LaTeX output from gt and ensures that it knits
#' well with `knitr`
#'
#' The `gt::as_latex` function is still in development, rather feature poor, and
#' prone to breakage when using `knitr`. This function is a stopgap measure
#' which adds a little functionality and "cleans-up" some of the LaTeX output to
#' avoid common compilation errors. In time, as upstream improves, the goal is
#' to deprecate this function.
#'
#' @param tab table object produced by `modelsummary` or `gt`
#' @param label string will be inserted as a `label`
#' @return an object of class `knit_asis`. The first element of this object
#'   (`x[[1]]`) contains raw LaTeX code.
#' @export
knit_latex <- function(tab, label = NULL) {

    # input sanity check
    checkmate::check_character(label, len = 1, null.ok = TRUE)

    # add empty title if label != NULL and there is no title in tab
    if (!is.null(label)) {
        if (is.null(attr(tab, 'heading'))) {
            tab <- tab %>%
                   gt::tab_header(title = '')
        }
    }

    # build latex
    out <- tab %>%
           gt::as_latex()

    # TODO: deprecate when fixed upstream -- gt group labels suppression
    out[[1]] <- stringr::str_replace_all(out[[1]],
                                         stringr::fixed("\\midrule\n\\multicolumn{1}{l}{} \\\\ \n\\midrule"),
                                         stringr::fixed("\\midrule"))
    out[[1]] <- stringr::str_replace_all(out[[1]],
                                         stringr::fixed("\\midrule\n\\multicolumn{1}{l}{\\vspace*{-5mm}} \\\\ \n\\midrule"),
                                         stringr::fixed("\\midrule"))

    # TODO: deprecate when fixed upstream -- empty subtitle breaks compilation
    out[[1]] <- stringr::str_replace(out[[1]],
                                     stringr::fixed('\\\\ \n\\small \\\\ \n'),
                                     '')

    # TODO: deprecate when fixed upstream -- longtable + booktabs + row beginning in ( or [
    out[[1]] <- stringr::str_replace_all(out[[1]],
                                         stringr::fixed("\\midrule"),
                                         stringr::fixed("\\midrule\\relax"))

    # TODO: deprecate when fixed upstream -- allow users to include caption labels
    if (!is.null(label)) {
        out[[1]] <- stringr::str_replace(out[[1]],
                                         'labelformat=empty',
                                         'labelformat=default')
        out[[1]] <- stringr::str_replace(out[[1]],
                                         stringr::fixed("\\caption*{"),
                                         stringr::fixed(paste0("\\caption{\\label{", label, "}")))
    }

    # output
    return(out)
}
