#' Beautiful, customizable summaries of statistical models
#'
#' @param models a single model object or a (potentially named) list of models
#' to summarize
#' @param statistic string name of the statistic to include in parentheses
#' below estimates. Must be either "conf.int", or one of the column names
#' produced by the `broom::tidy` function. Typical values include: "std.error",
#' "conf.int", "statistic", "p.value".
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `stringr::str_detect`).
#' @param coef_map named character vector. Names refer to the original variable
#' names. Values refer to the variable names that will appear in the table.
#' Coefficients which are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param gof string regular expression. Omits all matching gof statistics from
#' the table (using `stringr::str_detect`).
#' @param gof_map data.frame in the same format as `gtsummary::gof_map`
#' @param fmt passed to the `sprintf` function to format numeric values into
#' strings, with rounding, etc. See `?sprintf` for options.
#' @param notes a list of notes to append to the bottom of the table.
#'
#' @export
gtsummary <- function(models, 
                      statistic = 'std.error',
                      conf_level = 0.95,
                      coef_map = NULL,
                      coef_omit = NULL,
                      gof_map = NULL,
                      gof_omit = NULL,
                      fmt = '%.3f', 
                      stars = NULL,
                      title = NULL,
                      subtitle = NULL,
                      notes = NULL,
                      filename = NULL) {
    dat <- extract(models, 
                   statistic = statistic,
                   conf_level = conf_level,
                   coef_map = coef_map,
                   coef_omit = coef_omit,
                   gof_map = gof_map,
                   gof_omit = gof_omit,
                   stars = stars,
                   fmt = fmt)
    tab <- dat %>%
           # remove duplicate term labels
           dplyr::mutate(term = ifelse(statistic == 'statistic', '', term)) 
    idx <- (1:nrow(tab))[tab$group == 'estimates']
    tab <- tab %>%
           # remove columns not fit for printing
           dplyr::select(-statistic, -group) %>%
           ## group statistics (alternate mechanism. probably better, but I
           ## can't find a way to suppress group labels)
           #dplyr::group_by(group) %>%
           # gt object
           gt::gt(rowname_col = 'term') %>%
           # group statistics
           gt::tab_row_group(group = '', rows = idx)
    # titles
    if (!is.null(title)) {
        tab <- tab %>% gt::tab_header(title = title, subtitle = subtitle)
    }
    # stars
    if (!is.null(stars)) {
        if (is.logical(stars)) {
            if (stars) {
                stars <- c('*' = .1, '**' = .05, '***' = .01)
            }
        }
        stars <- sort(stars, decreasing = TRUE)
        stars_note <- paste0(names(stars), ' p < ', stars)
        stars_note <- paste(stars_note, collapse = ', ')
        tab = tab %>% gt::tab_source_note(source_note = stars_note)
    }
    # notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% gt::tab_source_note(source_note = n)
        }
    }
    # write to file 
    if (!is.null(filename)) {
        # guess output format based on filename extension
        ext <- tools::file_ext(filename)
        if (ext == 'html') {
            tab %>% gt::as_raw_html() %>%
                    cat(file = filename)
        } else if (ext == 'rtf') {
            tab %>% gt::as_rtf() %>%
                    cat(file = filename)
        } else if (ext == 'tex') {
            tab %>% gt::as_latex() %>%
                    cat(file = filename)
        } else {
            stop('filename must have one of the following extensions: .html, .rtf, .tex')
        }
    } else {
        return(tab)
    }
}
