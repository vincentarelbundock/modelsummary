# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'group', 'estimate', 'conf.high', 'conf.low', 'value', 'p.value', 'std.error', 'statistic'))

#' Beautiful, customizable summaries of statistical models
#'
#' @param models a single model object or a (potentially named) list of models
#' to summarize
#' @param output filename or object type (string)
#' \itemize{
#'   \item Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg. 
#'   \item Supported object types: "gt", "html", "markdown", "latex". "gt" objects are created by the `gt` package; other object types are created by the `kableExtra` package.
#'   \item When a file name is supplied to the `output` argument, the table is written immediately to file. If you want to customize your table by post-processing it with functions provided by the `gt` or `kableExtra` packages, you need to choose a different output format (e.g., "gt", "latex", "html", "markdown"), and you need to save the table after post-processing using the `gt::gtsave`, `kable::save_kable`, or `cat` functions.
#' }
#' @param fmt string which specifies how numeric values will be rounded. This
#' string is passed to the `sprintf` function. '\%.3f' will keep 3 digits after
#' the decimal point with trailing zero. '\%.5f' will keep 5 digits. '\%.3e' will
#' use exponential notation. See `?sprintf` for more options.
#' @param stars FALSE for no significance stars. TRUE for default significance
#' stars (*=.1, **=.05, ***=.01). Named numeric vector for custom significance
#' stars. For example, `c('*' = .1, '+' = .05)`
#' @param statistic string name of the statistic to include in parentheses
#' below estimates. Must be either "conf.int", or one of the column names
#' produced by the `broom::tidy` function. Typical values include: "std.error",
#' "conf.int", "statistic", "p.value". A character vector will stack several
#' uncertainty estimates on top of one another (in different rows).
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
#' @param add_rows list of character vectors, each of length equal to the number
#' of models + 1.
#' @param add_rows_location integer or NULL. custom rows will be added to the
#' bottom of the table if this parameter is NULL, or after the position set by
#' this integer.
#' @param title string
#' @param subtitle string
#' @param notes list of notes to append to the bottom of the table.
#' @param filename This argument was deprecated in favor of the `output` argument.
#' @param ... all other arguments are passed to the `tidy` method used to
#' extract estimates from the model. For example, this allows users to set
#' `exponentiate=TRUE` to exponentiate logistic regression coefficients.
#' @return a 'gt' table object.
#' @examples
#' \donttest{
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
#' # titles and subtitles
#' msummary(models, title = 'This is the title', subtitle = 'And a subtitle')
#'
#' # title with italicized text
#' msummary(models, title = gt::md('This is *the* title'))
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
                         output = "gt",
                         statistic = 'std.error',
                         statistic_override = NULL,
                         statistic_vertical = TRUE,
                         conf_level = 0.95,
                         coef_map = NULL,
                         coef_omit = NULL,
                         gof_map = modelsummary::gof_map,
                         gof_omit = NULL,
                         fmt = '%.3f',
                         stars = FALSE,
                         title = NULL,
                         subtitle = NULL,
                         notes = NULL,
                         add_rows = NULL,
                         add_rows_location = NULL,
                         filename = NULL,
                         ...) {

    # deprecation warning
    if (!is.null(filename)) {
        stop('The `filename` argument has been deprecated. Please use `output` instead.') 
    }

    # models must be a list of models or a single model
    if (!'list' %in% class(models)) {
        models <- list(models)
    }

    # check sanity of user input
    sanity_checks(models,
                  statistic = statistic,
                  statistic_override = statistic_override,
                  statistic_vertical = statistic_vertical,
                  conf_level = conf_level,
                  coef_map = coef_map,
                  coef_omit = coef_omit,
                  gof_map = gof_map,
                  gof_omit = gof_omit,
                  fmt = fmt,
                  stars = stars,
                  title = title,
                  subtitle = subtitle,
                  notes = notes,
                  add_rows = add_rows,
                  output = output)

    # extract estimates and gof
    dat <- modelsummary::extract(models,
                              statistic = statistic,
                              statistic_override = statistic_override,
                              statistic_vertical = statistic_vertical,
                              conf_level = conf_level,
                              coef_map = coef_map,
                              coef_omit = coef_omit,
                              gof_map = gof_map,
                              gof_omit = gof_omit,
                              stars = stars,
                              add_rows = add_rows,
                              add_rows_location = add_rows_location,
                              fmt = fmt,
                              ...)

    # remove duplicate term labels
    idx <- stringr::str_detect(dat$statistic, 'statistic\\d*$')
    tab <- dat %>%
           dplyr::mutate(term = ifelse(idx, '', term))

    # write to file or console? with gt or kableExtra? 
    if (output == 'gt') {
        build_table <- build_gt
    } else if (output %in% c('markdown', 'latex', 'html')) {
        build_table <- build_kableExtra
    } else {
        ext <- tools::file_ext(output)
        if (ext %in% c('html', 'jpg', 'png', 'rtf')) {
            build_table <- build_gt
        } else if (ext %in% c('tex', 'md', 'txt')) {
            build_table <- build_kableExtra
        }
    }

    # build table
    build_table(tab, 
                title = title,
                subtitle = subtitle,
                stars = stars,
                stars_note = stars_note,
                notes = notes,
                filename = filename,
                output = output,
                ...)

}

#' Beautiful, customizable summaries of statistical models
#'
#' `msummary()` is a shortcut to `modelsummary()`
#' @inherit modelsummary
#' @export
msummary <- modelsummary

