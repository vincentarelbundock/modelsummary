#' sanity check
#'
#' @keywords internal
sanity_estimate <- function(estimate) checkmate::assert_character(estimate)

#' sanity check
#'
#' @keywords internal
sanity_title <- function(title) checkmate::assert_character(title, len = 1, null.ok = TRUE)

#' sanity check
#'
#' @keywords internal
sanity_coef_map <- function(coef_map) checkmate::assert_character(coef_map, null.ok = TRUE)

#' sanity check
#'
#' @keywords internal
sanity_coef_omit <- function(coef_omit) checkmate::assert_string(coef_omit, null.ok = TRUE)

#' sanity check
#'
#' @keywords internal
sanity_gof_omit <- function(gof_omit) checkmate::assert_string(gof_omit, null.ok = TRUE)

#' sanity check
#'
#' @keywords internal
sanity_gof_map <- function(gof_map) checkmate::check_data_frame(gof_map, null.ok = TRUE)

#' sanity check
#'
#' @keywords internal
sanity_fmt <- function(fmt) checkmate::assert_string(fmt, null.ok = FALSE)

#' sanity check
#'
#' @keywords internal
sanity_conf_level <- function(conf_level) {
    checkmate::assert_number(conf_level, lower = 0, upper = .999999999999)
}

#' sanity check
#'
#' @keywords internal
sanity_stars <- function(stars) {
    checkmate::assert(
        checkmate::check_flag(stars),
        checkmate::check_numeric(stars, lower = 0, upper = 1, names = 'unique')
    )
}

#' sanity check
#'
#' @keywords internal
sanity_notes <- function(notes) {
    checkmate::assert(
        checkmate::check_list(notes, null.ok = TRUE),
        checkmate::check_character(notes)
    )
    if ('list' %in% class(notes)) {
        for (note in notes) {
            checkmate::assert(
                checkmate::check_character(note),
                checkmate::check_class(note, 'from_markdown')
            )
        }
    }
}

#' sanity check
#'
#' @keywords internal
sanity_output <- function(output) {
    checkmate::assert_string(output)
    cond1 <- output %in% c('default', 'gt', 'markdown', 'html', 'latex')
    if (isFALSE(cond1)) {
        extension <- tools::file_ext(output)
        cond2 <- extension %in% c('htm', 'html', 'tex', 'jpg', 'png', 'md', 'txt', 'rtf')
        if (isTRUE(cond2)) {
            checkmate::assert_path_for_output(output)
        } else {
            msg <- stop('The `output` argument must be "gt", "markdown", "html", "latex", or a valid file name with one of these extensions: ".html", ".tex", ".rtf", ".md", ".txt", ".jpg", ".png"')
        }
    }
}

#' sanity check
#'
#' @keywords internal
sanity_add_rows <- function(add_rows, add_rows_location, models) {
    checkmate::assert_list(add_rows, null.ok = TRUE)
    checkmate::assert_number(add_rows_location, null.ok = TRUE)
    if ('list' %in% class(add_rows)) {
        for (custom_row in add_rows) {
            checkmate::assert_character(custom_row, null.ok = FALSE, len = (length(models) + 1))
        }
    }
}

#' sanity check
#'
#' @keywords internal
sanity_statistic <- function(statistic,
                             statistic_override,
                             statistic_vertical,
                             models) {

    checkmate::assert_character(statistic)

    checkmate::assert(checkmate::check_list(statistic_override, null.ok = TRUE),
                      checkmate::check_function(statistic_override, null.ok = TRUE))

    if (is.list(statistic_override)) {
        checkmate::assert_true(length(statistic_override) == length(models))
        checkmate::assert(checkmate::check_true(all(sapply(statistic_override, is.function))),
                          checkmate::check_true(all(sapply(statistic_override, is.vector))),
                          checkmate::check_true(all(sapply(statistic_override, is.matrix))))
    } else if (is.function(statistic_override)) {
        statistic_override <- lapply(models, function(x) statistic_override)
    }

    # statistic_vertical = FALSE: only one statistic can be displayed horizontally
    checkmate::assert_logical(statistic_vertical, len = 1, null.ok = FALSE)
    if (!statistic_vertical) {
      if (length(statistic) > 1 | (length(statistic_override) > 1) & !is.vector(statistic_override[1])) {
        stop("Only one statistic can be displayed next to the estimate. Check the statistic_vertical argument.")

      }
    }
}

#' sanity check
#'
#' @keywords internal
sanity_filename <- function(filename) {
    if (!is.null(filename)) {
        stop('The `filename` argument is deprecated. Please use `output` instead.') 
    }
}

#' sanity check
#'
#' @keywords internal
sanity_subtitle <- function(subtitle) {
    if (!is.null(subtitle)) {
        stop('The `subtitle` argument is deprecated. If you want to add a subtitle to an HTML table, you can use the `tab_header` function from the `gt` package.') 
    }
}

#' sanity check
#'
#' @keywords internal
sanity_tidy_output <- function(tidy_output, estimate, statistic, modelclass) {
    cond1 <- checkmate::check_data_frame(tidy_output, min.rows = 1, min.cols = 3)
    cond2 <- FALSE
    statistic[statistic == 'conf.int'] <- 'conf.low'
    if (isTRUE(cond1)) {
        cond2 <- all(c('term', estimate, statistic) %in% colnames(tidy_output))
    }
    if (!isTRUE(cond2)) {
        stop('You tried to summarize a model of class: ', modelclass, '. Unfortunately, `tidy(model)` did not produce a dataframe with columns terms, ', estimate, ', and ', statistic, '. Make you this kind of model is supported by `broom`, `broom.mixed`, or some other loaded R package. Alternatively, try modifying the `modelsummary` `estimate` and `statistic` argument to match a column name in `tidy(model)`')
    }
}
