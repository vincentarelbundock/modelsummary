#' Internal function to build table with `huxtable`
#'
#' @inheritParams factory_gt
#' @keywords internal
#' @return huxtable object
factory_huxtable <- function(tab,
                             align = NULL,
                             hrule = NULL,
                             notes = NULL,
                             output_file = NULL,
                             output_format = 'huxtable',
                             title = NULL,
                             ...) {


    # is huxtable installed?
    if (!requireNamespace('huxtable', quietly = TRUE)) {
        stop("Please install the `huxtable` package.")
    }

    ## warning on align
    #if (!is.null(align)) {
        #warning('The `align` argument is not supported yet for huxtable objects. Please file a request or (even better) a code submission on Github if you need that feature.')
    #}

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
                                       value = 1)

    if (!is.null(hrule)) {
        for (pos in hrule) {
            out <- out %>% 
                   huxtable::set_bottom_border(row = pos, 
                                               col = 1:ncol(.),
                                               value = 1)
        }
    }

    # title
    if (!is.null(title)) {
        out <- huxtable::set_caption(out, title)
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            out <- huxtable::add_footnote(out, text = n)
        }
    }

    # output
    if (is.null(output_file)) {
        return(out)
    } else {
        if (output_format == 'word') {
            huxtable::quick_docx(out, file = output_file, open = FALSE)
        } else if (output_format == 'powerpoint') {
            huxtable::quick_pptx(out, file = output_file, open = FALSE)
        } else if (output_format == 'html') {
            huxtable::quick_html(out, file = output_file, open = FALSE)
        } else if (output_format == 'rtf') {
            huxtable::quick_rtf(out, file = output_file, open = FALSE)
        } else if (output_format == 'latex') {
            huxtable::quick_rtf(out, file = output_file, open = FALSE)
        }
    }

}
