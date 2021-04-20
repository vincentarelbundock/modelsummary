#' assert if dependency is installed
#'
#' @noRd
assert_dependency <- function(library_name, msg = NULL) {
  if (is.null(msg)) {
    msg <- "Please install the %s package."
  }
  if (!requireNamespace(library_name, quietly = TRUE)) {
    stop(sprintf(msg, library_name))
  }
}


#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  requireNamespace(library_name, quietly = TRUE)
}


#' sanity check
#'
#' @noRd
sanity_group_map <- function(group_map) {
  if (!is.null(group_map)) {
    if (is.null(names(group_map))) {
      checkmate::assert_character(group_map, unique = TRUE)
    } else {
      checkmate::assert_character(names(group_map), null.ok = TRUE, unique = TRUE)
    }
  }
}

#' sanity check
#'
#' @noRd
sanity_ellipsis <- function(vcov, ...) {
  ellip <- list(...)

  if ("statistic_vertical" %in% names(ellip)) {
    warning("The `statistic_vertical` argument is deprecated and will be ignored. To display uncertainty estimates next to your coefficients, use a `glue` string in the `estimate` argument. See `?modelsummary`")
  }

  if (!is.null(vcov) && ("statistic_override" %in% names(ellip))) {
    stop("The `vcov` and `statistic_override` arguments cannot be used at the same time. The `statistic_override` argument is deprecated. Please use `vcov` instead.")
  }
}


#' sanity check
#'
#' @noRd
sanity_model_names <- function(modelnames) {
  if (any(modelnames == '')) {
    stop('Model names cannot include empty strings. Please make sure that every object in the `models` list has a unique, non-empty name. If the `models` list has no names at all (NULL), `modelsummary` will create some automatically.')
  }
}


#' sanity check
#'
#' @noRd
sanity_align <- function(align, tab) {
  checkmate::assert(
    checkmate::check_character(align, len = 1, null.ok = TRUE),
    checkmate::check_character(align, len = ncol(tab), null.ok = TRUE),
    combine = "or"
  )
}


#' sanity check
#'
#' @noRd
sanitize_estimate <- function(estimate, number_of_models) {
  checkmate::assert(
    checkmate::check_character(estimate, len = 1),
    checkmate::check_character(estimate, len = number_of_models))

  if (length(estimate) == 1) {
    out <- rep(estimate, number_of_models)
    out <- as.list(out)
  } else {
    out <- as.list(estimate)
  }

  return(out)
}


#' sanity_check
#'
#' @noRd
sanity_statistic <- function(statistic) {
  checkmate::assert_character(statistic, null.ok = TRUE)
}


#' sanity check
#'
#' @noRd
sanity_title <- function(title) checkmate::assert_character(title, len = 1, null.ok = TRUE)


#' sanity check
#'
#' @noRd
sanity_coef <- function(coef_map, coef_rename, coef_omit) {

  checkmate::assert_string(coef_omit, null.ok = TRUE)

  if (!is.null(coef_rename) & !is.null(coef_map)) {
    stop("coef_map and coef_rename cannot be used together.")
  }

  if (!is.null(coef_map)) {
    if (is.null(names(coef_map))) {
      checkmate::assert_character(coef_map, null.ok = TRUE, unique = TRUE)
    } else {
      checkmate::assert_character(names(coef_map), null.ok = TRUE, unique = TRUE)
    }
  }

  if (is.character(coef_rename)) {
    checkmate::assert_character(coef_rename, null.ok = TRUE, names = "unique")
  } else {
    checkmate::assert_function(coef_rename, null.ok = TRUE)
  }
}


#' sanity check
#'
#' @noRd
sanity_gof_map <- function(gof_map, gof_omit) {
  checkmate::assert_string(gof_omit, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_data_frame(gof_map, null.ok = TRUE, min.cols = 3),
    checkmate::check_list(gof_map, null.ok = TRUE),
    combine = "or")
  if (class(gof_map)[1] == "list") {
    for (gm in gof_map) {
      checkmate::assert_list(gm, len = 3, names = "named")
      checkmate::assert_true(all(c("raw", "clean", "fmt") %in% names(gm)))
    }
  }
}


#' sanity check
#'
#' @noRd
sanity_fmt <- function(fmt) {
  checkmate::assert(
    checkmate::check_character(fmt, len = 1),
    checkmate::check_numeric(fmt, len = 1, lower = 0),
    checkmate::check_function(fmt)
  )
}


#' sanity check
#'
#' @noRd
sanity_conf_level <- function(conf_level) {
  checkmate::assert_number(conf_level, lower = 0, upper = .999999999999, null.ok = TRUE)
}


#' sanity check
#'
#' @noRd
sanity_factory <- function(factory_dict) {
  check_option <- function(output_type, valid) {
    if (!factory_dict[output_type] %in% valid) {
      msg <- sprintf("`modelsummary` cannot write a table of type '%s' using the '%s' package. You must use one of the following options: %s. Consider setting a global option such as: option(modelsummary_%s=%s)",
        output_type,
        factory_dict[output_type],
        paste(valid, collapse = ', '),
        valid[1],
        output_type)
      stop(msg)
    }
  }
  check_option('html', c('gt', 'kableExtra', 'flextable', 'huxtable'))
  check_option('rtf', c('gt', 'huxtable'))
  check_option('latex', c('gt', 'kableExtra', 'huxtable'))
  check_option('markdown', c('kableExtra'))
  check_option('word', c('flextable', 'huxtable'))
  check_option('powerpoint', c('flextable', 'huxtable'))
  check_option('png', c('gt', 'flextable', 'kableExtra'))
  check_option('jpg', c('flextable', 'kableExtra'))

  # check default
  modelsummary_default <- getOption("modelsummary_default", default = "kableExtra")
  checkmate::assert_true(modelsummary_default %in% c("gt", "kableExtra",
                                                     "flextable", "huxtable",
                                                     "jupyter", "markdown",
                                                     "html", "data.frame",
                                                     "dataframe", "latex",
                                                     "latex_tabular"))
}


#' sanity check
#'
#' @noRd
sanity_stars <- function(stars) {
  checkmate::assert(
    checkmate::check_flag(stars),
    checkmate::check_numeric(stars, lower = 0, upper = 1, names = 'unique')
  )
}


#' sanity check
#'
#' @noRd
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
#' @noRd
sanity_output <- function(output) {

  object_types <- c('default', 'gt', 'kableExtra', 'flextable', 'huxtable',
                    'html', 'jupyter', 'latex', 'latex_tabular', 'markdown',
                    'dataframe', 'data.frame', 'modelsummary_list')
  extension_types <- c('html', 'tex', 'md', 'txt', 'docx', 'pptx', 'rtf',
                       'jpg', 'png')

  checkmate::assert_string(output)

  cond1 <- output %in% object_types
  if (isFALSE(cond1)) {
    extension <- tools::file_ext(output)
    cond2 <- extension %in% extension_types
    if (isTRUE(cond2)) {
      checkmate::assert_path_for_output(output, overwrite = TRUE)
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
#' @noRd
sanity_add_rows <- function(add_rows, models) {
  if (inherits(add_rows, 'list')) {
    for (i in seq_along(add_rows)) {
      checkmate::assert_character(add_rows[[i]], null.ok = FALSE, len = (length(models) + 1))
    }
  } else if (inherits(add_rows, 'data.frame')) {
    checkmate::assert_true(all(c('section', 'position') %in% colnames(add_rows)))
    checkmate::assert_true(all(
      colnames(add_rows) %in% c('term', 'section', 'position', names(models))))
  }
}


#' sanity check
#'
#' @noRd
sanity_gof <- function(gof_output, gof_custom) {
  checkmate::assert_data_frame(gof_output, nrows = 1, null.ok = FALSE)
  checkmate::assert_data_frame(gof_custom, nrows = 1, null.ok = TRUE)
}

#' sanity check
#'
#' @noRd
sanity_tidy <- function(tidy_output, tidy_custom, estimate, statistic, modelclass) {

  # tidy model
  checkmate::assert_data_frame(tidy_output, min.rows = 1, min.cols = 3)
  checkmate::assert_true('term' %in% colnames(tidy_output))

  # tidy_custom model
  if (!is.null(tidy_custom)) {
    checkmate::assert_data_frame(tidy_custom,
      min.rows = 1, min.cols = 2)
    checkmate::assert_true('term' %in% colnames(tidy_custom))
  }
}

#' sanity check: datasummary
#'
#' @noRd
sanity_ds_nesting_factor <- function(formula, data) {
  idx <- sapply(data, function(x) is.character(x) | is.logical(x))
  idx <- names(idx)[idx]
  idx <- c(paste0('^', idx, ':'), paste0(':', idx, '$'))
  termlabs <- labels(stats::terms(formula))
  warn <- any(sapply(idx, function(x) any(grepl(x, termlabs))))
  if (warn) {
    warning('You are trying to create a nested table by applying the * operator to a character or a logical variable. It is usually a good idea to convert such variables to a factor before calling datasummary: dat$y<-as.factor(dat$y). Alternatively, you could wrap your categorical variable inside Factor() in the datasummary call itself: datasummary(x ~ Factor(y) * z, data)\n')
  }
}

#' sanity check: datasummary_balance
#'
#' right-handed formulae only
#' @noRd
sanity_ds_right_handed_formula <- function(formula) {
  termlabels <- labels(stats::terms(formula))
  if (length(termlabels) > 1) {
    stop("The 'datasummary_table' function only accepts a single right-hand side variable of type factor, character, or logical. If you do not want to transform your variable in the original data, you can wrap it in a Factor() call: datasummary_balance(~Factor(x), data). the name of your variablePlease visit the `modelsummary` website to learn how to build your own, more complex, Table 1. It's easy, I promise! https://vincentarelbundock.github.io/modelsummary/datasummary.html")
  }
}
