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
                           gof_idx,
                           output,
                           ...) {

    # huxtable object
    out <- huxtable::hux(tab)

    # title
    if (!is.null(title)) {
        out <- huxtable::set_caption(out, title)
    }

    # horizontal rule to separate coef/gof
    out <- huxtable::set_bottom_border(out, 
                                       row = gof_idx, 
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
