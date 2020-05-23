#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return tbl_gt object
build_kableExtra <- function(tab,
                             title,
                             subtitle,
                             stars,
                             stars_note,
                             notes,
                             filename,
                             output,
                             ...) {

    # output type
    output_latex <- (output == 'latex') | (tools::file_ext(output) == 'tex')
    output_markdown <- (output == 'markdown') | (tools::file_ext(output) %in% c('md', 'txt'))
    output_html <- (output == 'html') | (tools::file_ext(output) %in% c('htm', 'html'))

    # clean and measure table
    idx_row <- match('gof', tab$group)
    tab <- tab %>%
           dplyr::select(-statistic, -group) %>%
           dplyr::rename(`       ` = term) # HACK: arbitrary 7 spaces to avoid name conflict
    idx_col <- ncol(tab)

    # kable object type based on output 

    if (output_latex) {
        tab <- kableExtra::kable(tab, format = 'latex', caption = title,
                                 booktabs = TRUE, linesep = "")

    } else if (output_markdown) {
        tab <- kableExtra::kable(tab, format = 'markdown', caption = title)

    } else if (output_html) {
        tab <- kableExtra::kable(tab, format = 'html', caption = title)
    } 

    # horizontal rule to separate coef/gof
    if (output_latex) {
        if (!is.na(idx_row)) { # check if there are >0 GOF
            tab <- tab %>%
                   kableExtra::row_spec(idx_row - 1, extra_latex_after = '\\midrule')
        }
    }

    # stars note
    stars_note <- make_stars_note(stars)
    if (!is.null(stars_note)) {
        tab <- tab %>% 
               kableExtra::add_footnote(label = stars_note, notation = 'none')
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% 
                   kableExtra::add_footnote(label = n, notation = 'none')
        }
    }

    # output
    ext <- tools::file_ext(output)
    if (output %in% c('markdown', 'html', 'latex')) {
        return(tab)
    } else {
        sink(output)
        print(tab)
        sink()
    } 

}
