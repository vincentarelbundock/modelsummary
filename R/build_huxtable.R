#' Internal function to build table with `huxtable`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return huxtable object
build_huxtable <- function(tab,
                           title,
                           stars,
                           stars_note,
                           notes,
                           output,
                           ...) {

    # clean and measure table
    idx_row <- match('gof', tab$group)
    tab <- tab %>%
           dplyr::select(-statistic, -group) %>%
           # HACK: arbitrary 7 spaces to avoid name conflict
           dplyr::rename(`       ` = term)
    idx_col <- ncol(tab)

    # huxtable object
    out <- hux(tab)

    # title
    if (!is.null(title)) {
        out <- huxtable::set_caption(out, title)
    }

    # horizontal rule to separate coef/gof
    out <- huxtable::set_bottom_border(out, 
                                       row = idx_row, 
                                       col = 1:ncol(out),
                                       value = 1)

    # stars note
    stars_note <- make_stars_note(stars)
    if (!is.null(stars_note)) {
        out <- huxtable::add_footnote(out,
                                      text = stars_note)
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            out <- huxtable::add_footnote(out, text = n)
        }
    }

    return(out)

}
