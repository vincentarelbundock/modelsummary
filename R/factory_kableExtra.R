#' Internal function to build table with `kableExtra`
#'
#' @inheritParams factory_gt
#' @keywords internal
#' @return kableExtra object
factory_kableExtra <- function(tab,
                               align = NULL,
                               hrule = NULL,
                               notes = NULL,
                               output_file = NULL,
                               output_format = 'kableExtra',
                               title = NULL,
                               ...) {

    if (is.null(output_format) || 
        !output_format %in% c("latex", "markdown")) {
      output_format <- "html"
    }

    out <- kableExtra::kbl(
        tab,
        align = align,
        format = output_format,
        caption = title,
        booktabs = TRUE, 
        linesep = "",
        ...)

    # horizontal rule to separate coef/gof not supported in markdown
    # TODO: support HTML
    if (!is.null(hrule)) {
        if (output_format %in% 'latex') {
            for (pos in hrule) {
                out <- out %>% 
                       kableExtra::row_spec(row = pos - 1,  
                                            extra_latex_after = '\\midrule')
            }
        }
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        # threeparttable only works with 1 note. But it creates a weird bug
        # when using coef_map and stars in Rmarkdown PDF output
        for (n in notes) {
            out <- out %>% 
                   kableExtra::add_footnote(label = n, notation = 'none') 
        }
    }
    
    span <- attr(tab, 'span_kableExtra')
    if (!is.null(span)) {
        # add_header_above not supported in markdown
        if (output_format %in% c('latex', 'html')) {
            span <- rev(span) # correct vertical order
            for (s in span) {
                out <- out %>% kableExtra::add_header_above(s)
            }   
        }
    }

    # styling (can be overriden manually by calling again)
    if (output_format %in% c("latex", "html")) {
      out <- out %>% kableExtra::kable_styling(full_width = FALSE)
    }

    # output
    if (is.null(output_file)){
        return(out)
    } else {
        kableExtra::save_kable(out, file=output_file)
    } 

}
