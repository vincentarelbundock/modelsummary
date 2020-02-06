#' Utility function to cleanup LaTeX output from gt and ensures that it
#' compiles with latex and that it knits well with `knitr`
#'
#' The `gt::as_latex` function is still in development, rather feature poor, and
#' prone to breakage when using `knitr`. This function is a stopgap measure
#' which adds a little functionality and "cleans-up" some of the LaTeX output to
#' avoid common compilation errors. In time, as upstream improves, the goal is
#' to deprecate this function.
#' 
#' LaTeX compilation requires the following packages: booktabs, caption, longtable
#'
#' @param tab table object produced by `modelsummary` or `gt`
#' @param label string will be inserted as a `label`
#' @return an object of class `knit_asis`. The first element of this object
#'   (`x[[1]]`) contains raw LaTeX code.
#' @export
knit_latex <- function(tab, label=NULL) {
    # knitr is installed
    knitr_installed <- try(base::find.package('knitr'), silent = TRUE)
    knitr_installed <- !'try-error' %in% class(knitr_installed)
    if (!knitr_installed) {
        stop('The `knitr` package must be installed to use the `knit_latex` function.')
    }
    out <- clean_latex(tab, label = label)
    knitr::asis_output(out)
}

#' Utility function to cleanup LaTeX output from gt and ensures that it
#' compiles with latex
#'
#' The `gt::as_latex` function is still in development, rather feature poor,
#' and prone to breakage. This function is a stopgap measure which adds a
#' little functionality and "cleans-up" some of the LaTeX output to avoid
#' common compilation errors. In time, as upstream improves, the goal is to
#' deprecate this function.
#'
#' @param tab table object produced by `modelsummary` or `gt`
#' @param label string will be inserted as a `label`
#' @param gof_regex regex which identifies the first GOF statistic. Used to figure out
#' where to insert a midrule to separate coefficients from GOFs.
#' @return a string object with LaTeX code
#' @export
clean_latex <- function(tab, label = NULL, gof_regex = '^Num Obs.') {

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

    # caption should be normal size, not large
    out <- stringr::str_replace(out, '\\\\large ', '')
    
    # captionsetup options
    out <- stringr::str_replace(out, '1pt', '0pt')
    out <- stringr::str_replace(out, 'captionsetup\\[table\\]\\{', 'captionsetup\\[table\\]\\{font=normal,')
    out <- stringr::str_replace(out, 'labelformat=empty,', '')

    # midrule to separate coef/gof
    out <- stringr::str_replace(out, 'Num.Obs.', '\\\\midrule\nNum.Obs.')

    # center table
    out <- stringr::str_replace(out, 'longtable\\}', 'longtable\\}\\[c\\]')

    # tables should be numbered
    out <- stringr::str_replace(out, 'caption\\*', 'caption')

    # output
    return(out)
}
