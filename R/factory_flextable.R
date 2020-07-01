#' Internal function to build table with `flextable`
#'
#' @inheritParams factory_gt
#' @keywords internal
#' @return flextable object
factory_flextable <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              notes = NULL,
                              output_file = NULL,
                              output_format = 'flextable',
                              title = NULL,
                              ...) {


  
    # is flextable installed?
    if (!requireNamespace('flextable', quietly = TRUE)) {
        stop("Please install the `flextable` package.")
    }

    # measurements 
    table_width <- ncol(tab)

    # flextable object
    out <- flextable::flextable(tab)

    # title
    if (!is.null(title)) {
        out <- flextable::set_caption(out, title)
    }

    # horizontal rule to separate coef/gof
    if (!is.null(hrule)) {
        for (pos in hrule) {
            out <- flextable::border(out, 
                                     i = pos, 
                                     border.top = officer::fp_border())
        }
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (i in seq_along(notes)) {
            out <- flextable::add_footer_row(out,
                                             values = notes[[i]],
                                             colwidths = table_width)
        }
    }

    if (is.null(output_file)) {
        return(out)
    } else if (output_format == 'word') {
        flextable::save_as_docx(out, path = output_file)
    } else if (output_format == 'powerpoint') {
        flextable::save_as_pptx(out, path = output_file)
    } else if (output_format == 'png') {
        flextable::save_as_image(out, path = output_file)
    } else if (output_format == 'html') {
        flextable::save_as_html(out, path = output_file)
    }

}
