#' Internal function to build table with `huxtable`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return huxtable object
factory_huxtable <- function(tab,
                             title,
                             stars,
                             stars_note,
                             notes,
                             gof_idx,
                             output,
                             ...) {

    # is huxtable installed?
    if (!requireNamespace('huxtable', quietly = TRUE)) {
        stop("Please install the `huxtable` package.")
    }

    # huxtable object with header 
    out <- huxtable::hux(tab, add_colnames = TRUE) 

    # horizontal rules
    out <- out %>%
           huxtable::set_bottom_border(row = 1, 
                                       col = 1:ncol(.),
                                       value = 1) %>%
           huxtable::set_top_border(row = 1, 
                                    col = 1:ncol(.),
                                    value = 1) %>%
           huxtable::set_bottom_border(row = nrow(.), 
                                       col = 1:ncol(.),
                                       value = 1) %>%
           huxtable::set_bottom_border(row = gof_idx, 
                                       col = 1:ncol(.),
                                       value = 1)

    # title
    if (!is.null(title)) {
        out <- huxtable::set_caption(out, title)
    }


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

    # output
    ext <- tools::file_ext(output)
    if (output == 'huxtable') {
        return(out)
    } else if (ext == 'docx') {
        huxtable::quick_docx(out, file = output, open = FALSE)
    } else if (ext == 'pptx') {
        huxtable::quick_pptx(out, file = output, open = FALSE)
    } else if (ext %in% c('htm', 'html')) {
        huxtable::quick_html(out, file = output, open = FALSE)
    } else if (ext %in% c('rtf')) {
        huxtable::quick_rtf(out, file = output, open = FALSE)
    } else if (ext %in% c('tex')) {
        huxtable::quick_rtf(out, file = output, open = FALSE)
    }

}
