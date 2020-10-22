# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'group', 'estimate', 'conf.high', 'conf.low', 'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik', 'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model', 'tmp_grp', 'condition_variable'))


#' Beautiful, customizable summaries of statistical models
#'
#' @param models a single model object or a (potentially named) list of models
#' to summarize
#' @param output filename or object type (string)
#' \itemize{
#'   \item Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg.
#'   \item Supported object types: "default", "html", "markdown", "latex", "data.frame", "gt", "kableExtra", "huxtable", "flextable".
#'   \item When a file name is supplied to the `output` argument, the table is written immediately to file. If you want to customize your table by post-processing it with functions provided by the `gt` or `kableExtra` packages, you need to choose a different output format (e.g., "gt", "latex", "html", "markdown"), and you need to save the table after post-processing using the `gt::gtsave`, `kableExtra::save_kable`, or `cat` functions.
#' }
#' @param fmt an integer, string, or function which determines how to format numeric values:
#' \itemize{
#'   \item integer: the number of digits to keep after the period (`format(round(x, fmt), nsmall=fmt)`)
#'   \item character: string is passed to the `sprintf` function. '\%.3f' will keep 3 digits after the decimal point with trailing zero. '\%.5f' will keep 5 digits. '\%.3e' will use exponential notation. See `?sprintf` for more options.
#'   \item function: a function which returns a formatted character string
#' }
#' @param stars to indicate statistical significance
#' \itemize{
#'   \item FALSE (default): no significance stars.
#'   \item TRUE: *=.1, **=.05, ***=.01
#'   \item Named numeric vector for custom stars such as `c('*' = .1, '+' = .05)`
#' }
#' @param statistic string name of the statistic to include in parentheses
#' \itemize{
#'   \item Typical values: "conf.int", "std.error", "statistic", "p.value"
#'   \item Alternative values: any column name produced by `broom::tidy(model)`
#' }
#' @param statistic_override manually override the uncertainy estimates. This
#' argument accepts three types of input:
#' \itemize{
#'   \item a function or list of functions of length(models) which produce variance-covariance matrices with row and column names equal to the names of your coefficient estimates. For example, `R` supplies the `vcov` function, and the `sandwich` package supplies `vcovHC`, `vcovHAC`, etc.
#'   \item a list of length(models) variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#'   \item a list of length(models) vectors with names equal to the names of your coefficient estimates. Numeric vectors are formatted according to `fmt` and placed in brackets, character vectors printed as given.
#' }
#' @param statistic_vertical TRUE if statistics should be printed below
#' estimates. FALSE if statistics should be printed beside estimates.
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_map named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' Coefficients that are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `grepl(perl=TRUE)`).
#' @param coef_rename named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' @param gof_map data.frame with four columns: `raw`, `clean`, `fmt`, and
#' `omit`. If `gof_map` is NULL, then `modelsummary` will use this data frame
#' by default: `modelsummary::gof_map`
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `grepl(perl=TRUE)`).
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See examples.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate character name of the estimate to display. Must be a column
#' name in the data.frame produced by `tidy(model)`. In the vast majority of
#' cases, the default value of this argument should not be changed.
#' @param align A character string of length equal to the number of columns in
#' the table.  "lcr" means that the first column will be left-aligned, the 2nd
#' column center-aligned, and the 3rd column right-aligned.
#' @param ... all other arguments are passed to the `tidy` and `glance` methods
#' used to extract estimates from the model. For example, this allows users to
#' set `exponentiate=TRUE` to exponentiate logistic regression coefficients.
#' @return a regression table in a format determined by the `output` argument.
#' @examples
#' \dontrun{
#'
#' library(modelsummary)
#'
#' # load data and estimate models
#' data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#'
#' # simple table
#' modelsummary(models)
#'
#' # confidence intervals, p values, or t-stats instead of standard errors
#' modelsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' modelsummary(models, statistic = 'p.value', conf_level = 0.99)
#' modelsummary(models, statistic = 'statistic', conf_level = 0.99)
#'
#' # coef_rename: rename coefficients
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # coef_map: rename, re-order, and omit coefficients
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # titles
#' modelsummary(models, title = 'This is the title')
#'
#' # title with italicized text
#' modelsummary(models, title = gt::md('This is *the* title'))
#'
#' # add_rows: we use `tribble` from the `tibble` package to build a data.frame
#' # more easily. Then, we assign an attribute to determine each row's position.
#' rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
#'   'Empty row', '-', '-',
#'   'Another empty row', '?', '?')
#' attr(rows, 'position') <- c(1, 3)
#' modelsummary(models, add_rows = rows)
#'
#' # notes at the bottom of the table (here, the second note includes markdown bold characters)
#' modelsummary(models, notes = list('A first note', gt::md('A **bold** note')))
#'
#' # modify list of GOF statistics and their format using the built-in
#' # 'gof_map' data frame as a starting point
#' gof_custom <- modelsummary::gof_map
#' gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE
#' gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f"
#' modelsummary(models, gof_map = gof_custom)
#' }
#'
#' # see the README on github for a lot more examples:
#' # https://github.com/vincentarelbundock/modelsummary
#' @export
modelsummary <- function(models,
                         output = "default",
                         fmt = 3,
                         statistic = 'std.error',
                         statistic_override = NULL,
                         statistic_vertical = TRUE,
                         conf_level = 0.95,
                         stars = FALSE,
                         coef_map = NULL,
                         coef_omit = NULL,
                         coef_rename = NULL,
                         gof_map = NULL,
                         gof_omit = NULL,
                         add_rows = NULL,
                         title = NULL,
                         notes = NULL,
                         align = NULL,
                         estimate = 'estimate',
                         ...) {

  ellipsis <- list(...)

  # sanity checks for arguments not present in extract_models
  sanity_output(output)

  # extract estimates and gof
  dat <- extract_models(
    models,
    statistic = statistic,
    statistic_override = statistic_override,
    statistic_vertical = statistic_vertical,
    conf_level = conf_level,
    coef_map = coef_map,
    coef_omit = coef_omit,
    coef_rename = coef_rename,
    gof_map = gof_map,
    gof_omit = gof_omit,
    stars = stars,
    fmt = fmt,
    estimate = estimate,
    ...)

  # remove duplicate term labels
  idx <- grepl('statistic\\d*$', dat$statistic)
  tab <- dat %>%
    dplyr::mutate(term = ifelse(idx, '', term))

  # interaction : becomes ×
  if (is.null(coef_map)) {
    if (parse_output_arg(output)$output_format != 'rtf') {
      idx <- tab$group != 'gof'
      tab$term <- ifelse(idx, gsub(':', ' \u00d7 ', tab$term), tab$term)
    }
  }

  # measure table
  hrule <- match('gof', tab$group)
  if (!is.na(hrule) &&
    !is.null(add_rows) &&
    !is.null(attr(add_rows, 'position'))) {
    hrule <- hrule + sum(attr(add_rows, 'position') < hrule)
  }
  if (is.na(hrule)) {
    hrule <- NULL
  }

  # clean table but keep metadata for data.frame output
  if (parse_output_arg(output)$output_format != "dataframe") {
    tab <- tab %>%
      dplyr::select(-statistic, -group) %>%
      # HACK: arbitrary 7 spaces to avoid name conflict
      dplyr::rename(`       ` = term)
  }

  # stars
  if (!isFALSE(stars)) {
    stars_note <- make_stars_note(stars)
    if (is.null(notes)) {
      notes <- stars_note
    } else {
      notes <- c(stars_note, notes)
    }
  }

  # column alignment
  if (is.null(align)) {
    align <- paste0("l", strrep("c", ncol(tab) - 1))
  }

  # build table
  factory(tab,
    align = align,
    fmt = fmt,
    hrule = hrule,
    notes = notes,
    output = output,
    title = title,
    add_rows = add_rows,
    ...)

}

#' Beautiful, customizable summaries of statistical models
#'
#' `msummary()` is a shortcut to `modelsummary()`
#' @inherit modelsummary
#' @export
msummary <- modelsummary
