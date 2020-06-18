#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @param stars_note argument passed by `modelsummary()`
#' @keywords internal
#' @return tbl_gt object
factory_gt <- function(tab,
                       title = NULL,
                       stars = FALSE,
                       notes = NULL,
                       hrule = NULL,
                       span = NULL,
                       output_file,
                       output_format,
                       ...) {
  
    # create gt table object
    idx_col <- ncol(tab)
    tab <- tab %>% 
           gt::gt()

    # horizontal rule to separate coef/gof
    if (!is.null(hrule)) { # check if there are >0 GOF
        for (pos in hrule) {
            tab <- tab %>%
                   gt::tab_style(style = gt::cell_borders(sides = 'bottom', color = '#000000'),
                                 locations = gt::cells_body(columns = 1:idx_col, rows = (pos - 1)))
        }
    }

    # titles
    if (!is.null(title)) {
        tab <- tab %>% gt::tab_header(title = title)
    }

    # stars note
    if (!isFALSE(stars)) {
        stars_note <- make_stars_note(stars)
        tab = tab %>% gt::tab_source_note(source_note = stars_note)
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% gt::tab_source_note(source_note = n)
        }
    }
    
    # column span labels
    if (!is.null(span)) {
        for (s in span) {
            tab <- tab %>% gt::tab_spanner(label = s$label, columns = s$position)
        }
    }

    # output
    if (is.null(output_file)) {
        if (output_format == 'html') {
            return(as.character(gt::as_raw_html(tab)))
        } else if (output_format == 'latex') {
            return(as.character(gt::as_latex(tab))) 
        } else if (output_format %in% c('default', 'gt')) {
            return(tab)
        }
    } else {
        gt::gtsave(tab, output_file)
    }

}
