#' internal function to check the sanity of user input
#'
#' @return error if sanity checks fail
#' @inheritParams modelsummary
#' @keywords internal
sanity_checks <- function(models,
						  statistic = 'std.error',
						  statistic_override = NULL,
						  statistic_vertical = TRUE,
						  conf_level = 0.95,
						  coef_map = NULL,
						  coef_omit = NULL,
						  gof_map = NULL,
						  gof_omit = NULL,
						  fmt = '%.3f',
						  stars = NULL,
						  title = NULL,
						  notes = NULL,
						  add_rows = NULL,
                          output = NULL) {

    # simple parameters
    checkmate::assert_character(title, len = 1, null.ok = TRUE)
    checkmate::assert_character(statistic, null.ok = FALSE)
    checkmate::assert_character(coef_map, null.ok = TRUE)
    checkmate::assert_character(coef_omit, len = 1, null.ok = TRUE)
    checkmate::assert_character(gof_omit, len = 1, null.ok = TRUE)
    checkmate::assert_character(fmt, len = 1, null.ok = FALSE)

    # output 
    bad <- FALSE
    checkmate::assert_character(output, null.ok = FALSE)
    extension <- tools::file_ext(output)
    if (extension == '') {
        if (!output %in% c('default', 'gt', 'markdown', 'html', 'latex')) {
            bad <- TRUE
        }
    } else {
        if (!extension %in% c('', 'html', 'tex', 'jpg', 'png', 'md', 'txt', 'rtf')) {
            bad <- TRUE
        }
    }
    if (bad) {
        stop('The `output` argument must be "gt", "markdown", "html", "latex",
             or a valid file name with one of these extensions: ".html",
             ".tex", ".rtf", ".md", ".txt", ".jpg", ".png"')
    }

    # statistic_override
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


    # gof_map
    checkmate::assert(
        checkmate::check_data_frame(gof_map, null.ok = TRUE),
        checkmate::check_tibble(gof_map, null.ok = TRUE)
    )



    # stars
    checkmate::assert(
        checkmate::check_logical(stars, null.ok = FALSE),
        checkmate::check_numeric(stars, lower = 0, upper = 1, null.ok = FALSE)
    )

	# notes
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

	# add_rows
    checkmate::assert_list(add_rows, null.ok = TRUE)
    if ('list' %in% class(add_rows)) {
        for (custom_row in add_rows) {
            checkmate::assert_character(custom_row, null.ok = FALSE, len = (length(models) + 1))
        }
    }

}
