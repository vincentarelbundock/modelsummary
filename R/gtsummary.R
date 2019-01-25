#' Beautiful, customizable summaries of statistical models
#'
#' @param models a single model object or a (potentially named) list of models
#' to summarize
#' @param fmt string which specifies how numeric values will be rounded. This
#' string is passed to the `sprintf` function. '\%.3f' will keep 3 digits after
#' the decimal point with trailing zero. '\%.5f' will keep 5 digits. '\%.3e' will
#' use exponential notation. See `?sprintf` for more options.
#' @param filename if not NULL, the table will be written to file. The file
#' format is infered from the file extension. Valid extensions include: .html,
#' .rtf, .tex.
#' @param stars NULL for no significance stars. TRUE for default significance
#' stars (*=.1, **=.05, ***=.01). Named numeric vector for custom significance
#' stars: c('*' = .1, '+' = .05).
#' @param stars_note logical include a note at the bottom of the table to describe
#' the contents of the `stars` argument. The note will be omitted if `stars==NULL`
#' @param statistic string name of the statistic to include in parentheses
#' below estimates. Must be either "conf.int", or one of the column names
#' produced by the `broom::tidy` function. Typical values include: "std.error",
#' "conf.int", "statistic", "p.value".
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_map named character vector. Names refer to the original variable
#' names. Values refer to the variable names that will appear in the table.
#' Coefficients which are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `stringr::str_detect`).
#' @param gof_map data.frame in the same format as `gtsummary::gof_map`
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `stringr::str_detect`).
#' @param add_rows list of character vectors, each of length equal to the number
#' of models + 1.
#' @param title string
#' @param subtitle string
#' @param notes list of notes to append to the bottom of the table.
#' @examples
#'
#' # load data and estimate models
#' data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#'
#' # simple table
#' gtsummary(models)
#'
#' # confidence intervals, p values, or t-stats instead of standard errors
#' gtsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' gtsummary(models, statistic = 'p.value', conf_level = 0.99)
#' gtsummary(models, statistic = 'statistic', conf_level = 0.99)
#'
#' # rename and re-order coefficients
#' gtsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # save to file (html, rtf, or LaTeX)
#' gtsummary(models, filename = 'table.html')
#' gtsummary(models, filename = 'table.rtf')
#' gtsummary(models, filename = 'table.tex')
#'
#' # titles and subtitles
#' gtsummary(models, title = 'This is the title', subtitle = 'And a subtitle')
#'
#' # title with italicized text
#' gtsummary(models, title = gt::md('This is *the* title'))
#'
#' # notes at the bottom of the table (here, the second note includes markdown bold characters)
#' gtsummary(models, notes = list('A first note', gt::md('A **bold** note'))
#'
# see the README on github for a lot more examples: https://github.com/vincentarelbundock/gtsummary
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
                      stars_note = TRUE,
                      title = NULL,
                      subtitle = NULL,
                      notes = NULL,
                      add_rows = NULL,
                      filename = NULL) {

    # models must be a list of models or a single model
    if (!'list' %in% class(models)) {
        models <- list(models)
    }
    n_models <- length(models)

    # sanity checks for user inputs
    checkmate::assert_character(statistic, len = 1, null.ok = FALSE)
    checkmate::assert_character(coef_map, null.ok = TRUE)
    checkmate::assert_character(coef_omit, len = 1, null.ok = TRUE)
    checkmate::assert(
        checkmate::check_data_frame(gof_map, null.ok = TRUE),
        checkmate::check_tibble(gof_map, null.ok = TRUE)
    )
    checkmate::assert_character(gof_omit, len = 1, null.ok = TRUE)
    checkmate::assert_character(fmt, len = 1, null.ok = FALSE)
    checkmate::assert_character(title, len = 1, null.ok = TRUE)
    if (is.null(title)) {
        checkmate::assert_character(subtitle, len = 1, null.ok = TRUE)
    }
    checkmate::assert_character(subtitle, len = 1, null.ok = TRUE)
    checkmate::assert(
        checkmate::check_logical(stars, null.ok = TRUE),
        checkmate::check_numeric(stars, lower = 0, upper = 1, null.ok = TRUE)
    )
    checkmate::assert_logical(stars_note, null.ok = FALSE)
    checkmate::assert( # character vector or list of strings
        checkmate::check_list(notes, null.ok = TRUE),
        checkmate::check_character(notes, null.ok = TRUE)
    )
    if ('list' %in% class(notes)) {
        for (note in notes) {
            checkmate::assert(
                checkmate::check_character(note),
                checkmate::check_class(note, 'from_markdown')
            )
        }
    }
    checkmate::assert_list(add_rows, null.ok = TRUE)
    if ('list' %in% class(add_rows)) {
        for (custom_row in add_rows) {
            checkmate::assert_character(custom_row, null.ok = FALSE, len = (n_models + 1))
        }
    }
    checkmate::assert_character(filename, len = 1, null.ok = TRUE)

    # stars
    if (!is.null(stars)) {
        if (is.logical(stars)) {
            if (stars) {
                stars <- c('*' = .1, '**' = .05, '***' = .01)
            }
        }
        stars <- sort(stars, decreasing = TRUE)
    }

    # extract estimates and gof
    dat <- gtsummary::extract(models,
                              statistic = statistic,
                              conf_level = conf_level,
                              coef_map = coef_map,
                              coef_omit = coef_omit,
                              gof_map = gof_map,
                              gof_omit = gof_omit,
                              stars = stars,
                              add_rows = add_rows,
                              fmt = fmt)

    tab <- dat %>%
           # remove duplicate term labels
           dplyr::mutate(term = ifelse(statistic == 'statistic', '', term))

    # create gt table object
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

    # stars note
    if (stars_note & !is.null(stars)) {
        stars_note <- paste0(names(stars), ' p < ', stars)
        stars_note <- paste(stars_note, collapse = ', ')
        tab = tab %>%
              gt::tab_source_note(source_note = stars_note)
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% gt::tab_source_note(source_note = n)
        }
    }

    # output
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
