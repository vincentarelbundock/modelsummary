#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @param stars_note argument passed by `modelsummary()`
#' @keywords internal
#' @return tbl_gt object
build_gt <- function(tab,
                     title,
                     subtitle,
                     stars,
                     stars_note,
                     notes,
                     output,
                     ...) {

    # create gt table object
    idx_row <- match('gof', tab$group)
    idx_col <- ncol(tab) - 2
    tab <- tab %>%
           # remove columns not fit for printing
           dplyr::select(-statistic, -group) %>%
           # gt object
           dplyr::rename(`       ` = term) %>% # HACK: arbitrary 7 spaces to avoid name conflict
           gt::gt()

    # horizontal rule to separate coef/gof
    if (!is.na(idx_row)) { # check if there are >0 GOF
        tab <- tab %>%
               gt::tab_style(style = gt::cell_borders(sides = 'bottom', color = '#000000'),
                             locations = gt::cells_body(columns = 1:idx_col, rows = (idx_row - 1)))
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
    if (output %in% c('default', 'gt', 'html', 'rtf', 'latex')) {
        return(tab)
    } else {
        gt::gtsave(tab, output)
    }

}
