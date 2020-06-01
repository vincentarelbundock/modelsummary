#' Internal function to build table with `flextable`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return flextable object
build_flextable <- function(tab,
                            title,
                            stars,
                            stars_note,
                            notes,
                            gof_idx,
                            output,
                            ...) {
 
    table_width <- ncol(tab)

    # flextable object
    out <- flextable::flextable(tab)

    # title
    if (!is.null(title)) {
        out <- flextable::set_caption(out, title)
    }

    ## horizontal rule to separate coef/gof
    #out <- flextable::border(out, 
                              #row = gof_idx, 
                              #col = 1:ncol(out),
                              #value = 1)

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (i in length(notes):1) {
            out <- flextable::add_footer_row(out,
                                             values = notes[[i]],
                                             colwidths = table_width)
        }
    }

    # stars note
    stars_note <- make_stars_note(stars)
    if (!is.null(stars_note)) {
        out <- flextable::add_footer_row(out,
                                         values = stars_note,
                                         colwidths = table_width)
    }

    return(out)

}
