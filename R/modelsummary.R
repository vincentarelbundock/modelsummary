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
#'   \item When a file name is supplied to the `output` argument, the table is written immediately to file. If you want to customize your table by post-processing it with functions provided by the `gt` or `kableExtra` packages, you need to choose a different output format (e.g., "gt", "latex", "html", "markdown"), and you need to save the table after post-processing using the `gt::gtsave`, `kable::save_kable`, or `cat` functions.
#' }
#' @param fmt string which specifies how numeric values will be rounded. This
#' string is passed to the `sprintf` function. '\%.3f' will keep 3 digits after
#' the decimal point with trailing zero. '\%.5f' will keep 5 digits. '\%.3e' will
#' use exponential notation. See `?sprintf` for more options.
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
#' @param coef_map named character vector. Names refer to the original variable
#' names. Values refer to the variable names that will appear in the table.
#' Coefficients which are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `stringr::str_detect`).
#' @param gof_map data.frame with four columns: `raw`, `clean`, `fmt`, and
#' `omit`. See `modelsummary::gof_map`
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `stringr::str_detect`).
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See examples.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate character name of the estimate to display. Must be a column
#' name in the data.frame produced by `tidy(model)`. In the vast majority of
#' cases, the default value of this argument should not be changed.
#' @param ... all other arguments are passed to the `tidy` method used to
#' extract estimates from the model. For example, this allows users to set
#' `exponentiate=TRUE` to exponentiate logistic regression coefficients.
#' @return a 'gt' table object.
#' @examples
#' \donttest{
#' library(modelsummary)
#'
#' # load data and estimate models
#' data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#'
#' # simple table
#' msummary(models)
#'
#' # confidence intervals, p values, or t-stats instead of standard errors
#' msummary(models, statistic = 'conf.int', conf_level = 0.99)
#' msummary(models, statistic = 'p.value', conf_level = 0.99)
#' msummary(models, statistic = 'statistic', conf_level = 0.99)
#'
#' # rename and re-order coefficients
#' msummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # titles 
#' msummary(models, title = 'This is the title')
#'
#' # title with italicized text
#' msummary(models, title = gt::md('This is *the* title'))
#' 
#' # add_rows: we use `tribble` from the `tibble` package to build a data.frame
#' # more easily. Then, we assign an attribute to determine each row's position.
#' rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
#'                         'Empty row', '-', '-',
#'                         'Another empty row', '?', '?')
#' attr(rows, 'position') <- c(1, 3)
#' msummary(models, add_rows = rows)
#'
#' # notes at the bottom of the table (here, the second note includes markdown bold characters)
#' msummary(models, notes = list('A first note', gt::md('A **bold** note')))
#'
#' # modify list of GOF statistics and their format using the built-in
#' # 'gof_map' data frame as a starting point
#' gof_custom <- modelsummary::gof_map 
#' gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE 
#' gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f" 
#' msummary(models, gof_map = gof_custom)
#' }
#'
# see the README on github for a lot more examples: https://github.com/vincentarelbundock/modelsummary
#'
#' @export
modelsummary <- function(models,
                         output = "default",
                         fmt = '%.3f',
                         statistic = 'std.error',
                         statistic_override = NULL,
                         statistic_vertical = TRUE,
                         conf_level = 0.95,
                         stars = FALSE,
                         coef_map = NULL,
                         coef_omit = NULL,
                         gof_map = modelsummary::gof_map,
                         gof_omit = NULL,
                         add_rows = NULL,
                         title = NULL,
                         notes = NULL,
                         estimate = 'estimate',
                         ...) {


    # extract estimates and gof
    dat <- extract(models,
                   statistic = statistic,
                   statistic_override = statistic_override,
                   statistic_vertical = statistic_vertical,
                   conf_level = conf_level,
                   coef_map = coef_map,
                   coef_omit = coef_omit,
                   gof_map = gof_map,
                   gof_omit = gof_omit,
                   stars = stars,
                   fmt = fmt,
                   estimate = estimate,
                   ...)

    # remove duplicate term labels
    idx <- stringr::str_detect(dat$statistic, 'statistic\\d*$')
    tab <- dat %>%
           dplyr::mutate(term = ifelse(idx, '', term))

    # measure table
    hrule <- match('gof', tab$group)
    if (!is.null(add_rows) && !is.null(attr(add_rows, 'position'))) {
        hrule <- hrule + sum(attr(add_rows, 'position') < hrule)
    }

    # clean table
    tab <- tab %>%
           dplyr::select(-statistic, -group) %>%
           # HACK: arbitrary 7 spaces to avoid name conflict
           dplyr::rename(`       ` = term)


    # stars
    if (!isFALSE(stars)) {
        stars_note <- make_stars_note(stars)
        if (is.null(notes)) {
            notes <- stars_note
        } else {
            notes <- c(stars_note, notes)
        }
    }

    # column alignment left
    align <- strrep('l', ncol(tab))

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

