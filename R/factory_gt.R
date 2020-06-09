#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @param stars_note argument passed by `modelsummary()`
#' @keywords internal
#' @return tbl_gt object
factory_gt <- function(tab,
                       title,
                       subtitle,
                       stars,
                       stars_note,
                       notes,
                       gof_idx,
                       output,
                       ...) {
  
    # create gt table object
    idx_col <- ncol(tab)
    tab <- tab %>% 
           gt::gt()

    # horizontal rule to separate coef/gof
    if (!is.na(gof_idx)) { # check if there are >0 GOF
        tab <- tab %>%
               gt::tab_style(style = gt::cell_borders(sides = 'bottom', color = '#000000'),
                             locations = gt::cells_body(columns = 1:idx_col, rows = (gof_idx - 1)))
    }

    # titles
    if (!is.null(title)) {
        tab <- tab %>% gt::tab_header(title = title)
    }

    # stars note
    stars_note <- make_stars_note(stars)
    if (!is.null(stars_note)) {
        tab = tab %>% gt::tab_source_note(source_note = stars_note)
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% gt::tab_source_note(source_note = n)
        }
    }

    # output
    if (output == 'html') {
        return(as.character(gt::as_raw_html(tab)))
    } else if (output == 'latex') {
        return(as.character(gt::as_latex(tab))) 
    } else if (output %in% c('default', 'gt')) {
        return(tab)
    } else {
        gt::gtsave(tab, output)
    }

}
