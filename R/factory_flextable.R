#' Internal function to build table with `flextable`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return flextable object
factory_flextable <- function(tab,
                              title,
                              stars,
                              stars_note,
                              notes,
                              gof_idx,
                              output,
                              ...) {
  
    # is huxtable installed?
    if (!requireNamespace('flextable', quietly = TRUE)) {
        stop("Please install the `flextable` package.")
    }
 
    table_width <- ncol(tab)

    # flextable object
    out <- flextable::flextable(tab)

    # title
    if (!is.null(title)) {
        out <- flextable::set_caption(out, title)
    }

    # horizontal rule to separate coef/gof
    out <- flextable::border(out, 
                             i = gof_idx, 
                             border.top = officer::fp_border())

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

    ext <- tools::file_ext(output)
    if (output == 'flextable') {
        return(out)
    } else if (ext == 'docx') {
        flextable::save_as_docx(out, path = output)
    } else if (ext == 'pptx') {
        flextable::save_as_pptx(out, path = output)
    } else if (ext %in% c('png')) {
        flextable::save_as_image(out, path = output)
    } else if (ext %in% c('htm', 'html')) {
        flextable::save_as_html(out, path = output)
    }

}
