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
sanity_factory <- function(factory_dict) {
    check_option <- function(output_type, valid) {
        if (!factory_dict[[output_type]] %in% valid) {
            msg <- paste0("`modelsummary` cannot write a table of type '",
                          output_type,
                          "' using the ",
                          factory_dict[[output_type]],
                          " package. You must use one of the following packages: ",
                          paste(valid, collapse = ', '),
                          ". Consider setting a global option such as: option(modelsummary_", 
                          output_type,
                          "='",
                          valid[1],
                          "')"
                          )
            stop(msg)
        }
    }
    check_option('default', c('gt', 'kableExtra', 'flextable', 'huxtable'))
    check_option('html', c('gt', 'kableExtra', 'flextable', 'huxtable'))
    check_option('rtf', c('gt', 'huxtable'))
    check_option('latex', c('gt', 'kableExtra', 'huxtable'))
    check_option('markdown', c('kableExtra'))
    check_option('word', c('flextable', 'huxtable'))
    check_option('powerpoint', c('flextable', 'huxtable'))
    check_option('png', c('gt', 'flextable'))
    check_option('jpg', c('flextable'))
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

    object_types <- c('default', 'gt', 'kableExtra', 'flextable', 'huxtable', 'html', 'latex', 'markdown')
    extension_types <- c('html', 'tex', 'md', 'txt', 'docx', 'pptx', 'rtf', 'jpg', 'png')

    checkmate::assert_string(output)

    cond1 <- output %in% object_types
    if (isFALSE(cond1)) {
        extension <- tools::file_ext(output)
        cond2 <- extension %in% extension_types
        if (isTRUE(cond2)) {
            checkmate::assert_path_for_output(output)
        } else {
            msg <- paste0('The `output` argument must be ', 
                          paste(object_types, collapse = ', '),
                          ', or a valid file path with one of these extensions: ',
                          paste(extension_types, collapse = ', '))
            stop(msg)
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
sanity_gof <- function(gof_output, gof_custom) {
    checkmate::assert_data_frame(gof_output, nrows = 1, min.cols = 1, null.ok = FALSE)
    checkmate::assert_data_frame(gof_custom, nrows = 1, null.ok = TRUE)
}

#' sanity check
#'
#' @keywords internal
sanity_tidy <- function(tidy_output, tidy_custom, estimate, statistic, modelclass) {

    # tidy(model)
    checkmate::assert_data_frame(tidy_output, min.rows = 1, min.cols = 3)
    checkmate::assert_true('term' %in% colnames(tidy_output))

    # tidy_custom(model)
    checkmate::assert_data_frame(tidy_custom, nrows = nrow(tidy_output),
                                 min.cols = 2, null.ok = TRUE)

    if (!is.null(tidy_custom)) {
        checkmate::assert_true('term' %in% colnames(tidy_custom))
        checkmate::assert_true(all(tidy_output$term == tidy_custom$term))
    }

    # columns
    available <- c(colnames(tidy_output), colnames(tidy_custom))

    if (!estimate %in% available) {
        msg <- paste0('For models of class ',
                      modelclass,
                      ' the `estimate` argument of the `modelsummary()` function must be one of: ',
                      paste(available, collapse = ', '))
        stop(msg)
    }

    statistic[statistic == 'conf.int'] <- 'conf.low'
    if (!all(statistic %in% available)) {
        if ('conf.low' %in% available) {
            available <- base::setdiff(available, c('conf.low', 'conf.high'))
            available <- c('conf.int', available)
        }
        msg <- paste0('For models of class ',
                      modelclass,
                      ' the `statistic` argument of the `modelsummary()` function must be one of: ',
                      paste(available, collapse = ', '),
                      ' (or possibly conf.int)')
        stop(msg)
    }
}
