# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'part', 'estimate', 'conf.high', 'conf.low', 'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik', 'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model', 'tmp_grp', 'condition_variable'))



#' Simple, Beautiful, and Customizable Model Summaries
#'
#' @param models a model or (optionally named) list of models
#' @param output filename or object type (character string)
#' \itemize{
#'   \item Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg.
#'   \item Supported object types: "default", "html", "markdown", "latex", "data.frame", "gt", "kableExtra", "huxtable", "flextable".
#'   \item Warning: the `output` argument \emph{cannot} be used when customizing tables with external packages. See the 'Details' section below.
#' }
#' @param fmt determines how to format numeric values
#' \itemize{
#'   \item integer: the number of digits to keep after the period `format(round(x, fmt), nsmall=fmt)`
#'   \item character: passed to the `sprintf` function (e.g., '\%.3f' keeps 3 digits with trailing zero). See `?sprintf`
#'   \item function: returns a formatted character string.
#' }
#' @param stars to indicate statistical significance
#' \itemize{
#'   \item FALSE (default): no significance stars.
#'   \item TRUE: *=.1, **=.05, ***=.01
#'   \item Named numeric vector for custom stars such as `c('*' = .1, '+' = .05)`
#' }
#' @param statistic vector of strings or `glue` strings which select uncertainty statistics to report vertically below the estimate. NULL omits all uncertainty statistics. 
#' \itemize{
#'   \item "conf.int", "std.error", "statistic", "p.value", "conf.low", "conf.high", or any column name produced by: `get_estimates(model)`
#'   \item `glue` package strings with braces, such as: 
#'   \itemize{
#'     \item "\{p.value\} [\{conf.low\}, \{conf.high\}]"
#'     \item "Std.Error: \{std.error\}"
#'   }
#'   \item Note: Parentheses are added automatically unless the string includes `glue` curly braces \{\}.
#'   \item Note: To report uncertainty statistics \emph{next} to coefficients, you can supply a `glue` string to the `estimate` argument.
#' }
#' @param vcov robust standard errors and other manual statistics. The `vcov` argument accepts five types of input (see the 'Details' and 'Examples' sections below):
#' \itemize{
#'   \item string, vector, or list of strings: "robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "stata", or "classical" (alias "constant" or "iid").
#'   \item formula or list of formulas with the cluster variable(s) on the right-hand side (e.g., ~clusterid).
#'   \item function or list of functions which return variance-covariance matrices with row and column names equal to the names of your coefficient estimates (e.g., `stats::vcov`, `sandwich::vcovHC`).
#'   \item list of `length(models)` variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#'   \item a list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. 
#' }
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_map named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' Coefficients that are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table using `grepl(perl=TRUE)`.
#' @param coef_rename named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' @param gof_map 
#' \itemize{
#'   \item NULL (default): the `modelsummary::gof_map` dictionary is used for formatting, and all unknown statistic are included.
#'   \item data.frame with 3 columns named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples' section below.
#'   \item list of lists, each of which includes 3 elements named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples section below'.
#' }
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `grepl(perl=TRUE)`).
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See Examples section below.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate string or `glue` string of the estimate to display (or a
#' vector with one string per model). Valid entries include any column name of
#' the data.frame produced by `get_estimates(model)`. Examples:
#' \itemize{
#'   \item "estimate"
#'   \item "\{estimate\} (\{std.error\}){stars}"
#'   \item "\{estimate\} [\{conf.low\}, \{conf.high\}]"
#' }
#' @param align A character string of length equal to the number of columns in
#' the table.  "lcr" means that the first column will be left-aligned, the 2nd
#' column center-aligned, and the 3rd column right-aligned.
#' @param ... all other arguments are passed to the `tidy` and `glance` methods
#' used to extract estimates from the model. For example, this allows users to
#' set `exponentiate=TRUE` to exponentiate logistic regression coefficients.
#' @return a regression table in a format determined by the `output` argument.
#' @importFrom generics glance tidy
#' @details 
#' `output` argument:
#'
#' When a file name is supplied to the `output` argument, the table is written
#' immediately to file. If you want to customize your table by post-processing
#' it with an external package, you need to choose a different output format
#' and saving mechanism. Unfortunately, the approach differs from package to
#' package:
#' \itemize{
#'   \item `gt`: set `output="gt"`, post-process your table, and use the `gt::gtsave` function.
#'   \item `kableExtra`: set `output` to your destination format (e.g., "latex", "html", "markdown"), post-process your table, and use `kableExtra::save_kable` function.
#' }
#' 
#' `vcov` argument:
#'
#' To use a string such as "robust" or "HC0", your model must be supported
#' by the `sandwich` package. This includes objects such as: lm, glm,
#' survreg, coxph, mlogit, polr, hurdle, zeroinfl, and more.
#' 
#' "classical", "iid", and "constant" are aliases which do not modify
#' uncertainty estimates and simply report the default standard errors stored
#' in the model object.
#'
#' One-sided formulas such as `~clusterid` are passed to the `sandwich::vcovCL`
#' function.
#' 
#' Matrices and functions producing variance-covariance matrices are first
#' passed to `lmtest`. If this does not work, `modelsummary` attempts to take
#' the square root of the diagonal to adjust "std.error", but the other
#' uncertainty estimates are not be adjusted.
#'
#' Numeric vectors are formatted according to `fmt` and placed in brackets.
#' Character vectors printed as given, without parentheses. 
#'
#' If your model type is supported by the `lmtest` package, the
#' `vcov` argument will try to use that package to adjust all the
#' uncertainty estimates, including "std.error", "statistic", "p.value", and
#' "conf.int". If your model is not supported by `lmtest`, only the "std.error"
#' will be adjusted by, for example, taking the square root of the matrix's
#' diagonal.
#' @examples
#' \dontrun{
#'
#' # The `modelsummary` website includes \emph{many} examples and tutorials:
#' # https://vincentarelbundock.github.io/modelsummary
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
#' # statistic
#' modelsummary(models, statistic = NULL)
#' modelsummary(models, statistic = 'p.value')
#' modelsummary(models, statistic = 'statistic')
#' modelsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' modelsummary(models, statistic = c("t = {statistic}",
#'                                    "se = {std.error}",
#'                                    "conf.int"))
#' 
#' # estimate
#' modelsummary(models, 
#'   statistic = NULL,
#'   estimate = "{estimate} [{conf.low}, {conf.high}]")
#' modelsummary(models,
#'   estimate = c("{estimate}{stars}",
#'                "{estimate} ({std.error})"))
#' 
#' # vcov
#' modelsummary(models, vcov = "robust")
#' modelsummary(models, vcov = list("classical", "stata"))
#' modelsummary(models, vcov = sandwich::vcovHC)
#' modelsummary(models, 
#'   vcov = list(stats::vcov, sandwich::vcovHC))
#' modelsummary(models, 
#'   vcov = list(c("(Intercept)"="", "Height"="!"),
#'                             c("(Intercept)"="", "Height"="!", "Volume"="!!")))
#'
#' # coef_rename
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # coef_map
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # title
#' modelsummary(models, title = 'This is the title')
#'
#' # add_rows
#' rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
#'   'Empty row', '-', '-',
#'   'Another empty row', '?', '?')
#' attr(rows, 'position') <- c(1, 3)
#' modelsummary(models, add_rows = rows)
#'
#' # notes
#' modelsummary(models, notes = list('A first note', 'A second note'))
#'
#' # gof_map: data.frame
#' gm <- modelsummary::gof_map
#' gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE
#' gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f"
#' modelsummary(models, gof_map = gof_custom)
#' 
#' # gof_map: list of lists
#' f1 <- function(x) format(round(x, 3), big.mark=",")
#' f2 <- function(x) format(round(x, 0), big.mark=",")
#' gm <- list(
#'   list("raw" = "nobs", "clean" = "N", "fmt" = f2),
#'   list("raw" = "AIC", "clean" = "aic", "fmt" = f1))
#' modelsummary(models,
#'   fmt = f1,
#'   gof_map = gm)
#' 
#' }
#'
#' @export
modelsummary <- function(
  models,
  output      = "default",
  fmt         = 3,
  estimate    = "estimate",
  statistic   = "std.error",
  vcov        = NULL,
  conf_level  = 0.95,
  stars       = FALSE,
  coef_map    = NULL,
  coef_omit   = NULL,
  coef_rename = NULL,
  gof_map     = NULL,
  gof_omit    = NULL,
  add_rows    = NULL,
  align       = NULL,
  notes       = NULL,
  title       = NULL,
  ...) {


  # deprecated arguments

  ellip <- list(...)

  if ("statistic_vertical" %in% names(ellip)) {
    warning("The `statistic_vertical` argument is deprecated and will be ignored. To display uncertainty estimates next to your coefficients, use a `glue` string in the `estimate` argument. See `?modelsummary`")
  }

  if ("statistic_override" %in% names(ellip)) {
    if (!is.null(vcov)) {
      stop("The `vcov` and `statistic_override` arguments cannot be used at the same time. The `statistic_override` argument is deprecated. Please use `vcov` instead.")
    }
    vcov <- ellip$statistic_override
  }


  # models must be a list
  # first class because some models inherit from list
  # do this before sanity_vcov
  if (class(models)[1] != "list") { 
    models <- list(models)
  }


  # sanity check functions are hosted in R/sanity_checks.R
  sanity_output(output)
  sanity_statistic(statistic)
  sanity_estimate(models, estimate)
  sanity_conf_level(conf_level)
  sanity_coef(coef_map, coef_rename, coef_omit)
  sanity_gof_map(gof_map, gof_omit)
  sanity_stars(stars)
  sanity_fmt(fmt)

  if (!is.null(vcov)) {
    vcov <- sanitize_vcov(models, vcov)
  }


  # output
  output_format <- parse_output_arg(output)$output_format


  # estimate
  if (length(estimate) == 1) {
    estimate <- rep(estimate, length(models))
  }
  estimate <- as.list(estimate)


  # model names dictionary: use unique names for manipulation
  if (is.null(names(models))) {
    model_names <- paste("Model", 1:length(models))
  } else {
    model_names <- names(models)
  }
  model_id <- paste("Model", 1:length(models))


  # estimates: extract and combine
  est <- list()

  for (i in seq_along(models)) {

    tmp <- extract_estimates(
      model              = models[[i]],
      fmt                = fmt,
      estimate           = estimate[[i]],
      statistic          = statistic,
      vcov               = vcov[[i]],
      conf_level         = conf_level,
      stars              = stars,
      ...
    )

    # coef_rename: before merge to collapse rows
    if (!is.null(coef_rename)) {
      tmp$term <- replace_dict(tmp$term, coef_rename)
    }

    # coef_map
    if (!is.null(coef_map)) {
      tmp <- tmp[tmp$term %in% names(coef_map), , drop=FALSE]
      tmp$term <- replace_dict(tmp$term, coef_map)
    }

    # coef_omit
    if (!is.null(coef_omit)) {
      idx <- !grepl(coef_omit, tmp$term, perl=TRUE)
      tmp <- tmp[idx, , drop=FALSE]
    }

    # make sure no duplicate estimate names *within* a single model. this
    # cannot be in input sanity checks. idx paste allows multiple statistics.
    idx <- paste(tmp$term, tmp$statistic)
    if (anyDuplicated(idx) > 2) {
      stop('Two coefficients from a single model cannot share the same name. Check model ', i)
    }

    # model name is temporarily a unique id
    colnames(tmp)[3] <- model_id[i]

    # assign
    est[[model_id[i]]] <- tmp

  }

  term_order <- lapply(est, function(x) x$term)
  term_order <- unique(unlist(term_order))

  f <- function(x, y) merge(x, y, all=TRUE, sort=FALSE, by=c("term", "statistic"))
  est <- Reduce(f, est) 
  est$part <- "estimates"
  est <- est[, unique(c("part", "term", "statistic", names(est)))]

  # default order
  idx <- match(est[["term"]], term_order)
  est <- est[order(idx, est[["statistic"]]),]

  # coef_map
  if (!is.null(coef_map)) {
    coef_map <- intersect(coef_map, term_order)
    est <- est[est[["term"]] %in% coef_map, , drop=FALSE]
    idx <- match(est[["term"]], coef_map)
    est <- est[order(idx, est[["statistic"]]),]
  }

  # gof: extract and combine
  gof <- list()

  for (i in seq_along(models)) {
    gof[[i]] <- extract_gof(models[[i]], fmt=fmt, gof_map=gof_map, ...)
    colnames(gof[[i]])[2] <- model_id[i]
  }

  f <- function(x, y) merge(x, y, all=TRUE, sort=FALSE, by="term")
  gof <- Reduce(f, gof)

  if (nrow(gof) > 0) {

    # gof row identifier
    gof$part <- "gof"
    gof <- gof[, unique(c("part", "term", names(gof)))]

    # gof_omit
    if (!is.null(gof_omit)) {
      idx <- !grepl(gof_omit, gof$term, perl=TRUE)
      gof <- gof[idx, , drop=FALSE]
    }

    # gof_map
    if (nrow(gof) > 0) {
      if (is.null(gof_map)) {
        # assign here and not in the function definition because we use NULL to
        # figure out if F-stat should be included by default for lm models.
        gm_list <- get("gof_map", as.environment("package:modelsummary"))
        gm_list <- lapply(1:nrow(gm_list), function(i) gm_list[i, ])
      } else if (inherits(gof_map, "data.frame")) {
        gm_list <- lapply(1:nrow(gof_map), function(i) gof_map[i, ])
      } else {
        gm_list <- gof_map
      }
      gm_raw <- sapply(gm_list, function(x) x$raw)
      gm_clean <- sapply(gm_list, function(x) x$clean)
      gof_names <- gm_clean[match(gof$term, gm_raw)]
      gof_names[is.na(gof_names)] <- gof$term[is.na(gof_names)]
      gof$term <- gof_names
      idx <- match(gof$term, gm_clean)
      gof <- gof[order(idx, gof$term), ]
    }

  } 

  # combine estimates and gof
  if (nrow(gof) > 0) {
    tab <- bind_rows(est, gof)
  } else {
    tab <- est
  }

  # empty cells
  tab[is.na(tab)] <- ''

  # interaction : becomes ×
  if (is.null(coef_map)) {
    if (output_format != 'rtf') {
      idx <- tab$part != 'gof'
      tab$term <- ifelse(idx, gsub(':', ' \u00d7 ', tab$term), tab$term)
    }
  }

  # measure table
  hrule <- match('gof', tab$part)
  if (!is.na(hrule) &&
      !is.null(add_rows) &&
      !is.null(attr(add_rows, 'position'))) {
    hrule <- hrule + sum(attr(add_rows, 'position') < hrule)
  }
  if (is.na(hrule)) {
    hrule <- NULL
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


  # data.frame output keeps redundant info
  if (output_format != "dataframe") {

    # duplicate term labels
    idx <- grepl("modelsummary_tmp\\d+$", tab$statistic) &
           tab$statistic != "modelsummary_tmp1"
    tab$term[idx] <- ""

    # clean table but keep metadata for data.frame output
    if (output_format != "dataframe") {
      tab$statistic <- tab$part <- NULL

      # HACK: arbitrary 7 spaces to avoid name conflict
      colnames(tab)[colnames(tab)=="term"] <- "       "
    }

  }

  # column alignment (after removing extraneous columns)
  if (is.null(align)) {
    align <- paste0("l", strrep("c", ncol(tab) - 1))
  }

  # restore original model names
  idx <- match(model_id, colnames(tab))
  colnames(tab)[idx] <- model_names

  # build table
  factory(
    tab,
    align    = align,
    fmt      = fmt,
    hrule    = hrule,
    notes    = notes,
    output   = output,
    title    = title,
    add_rows = add_rows,
    ...
  )

}

#' Beautiful, customizable summaries of statistical models
#'
#' `msummary()` is a shortcut to `modelsummary()`
#' @inherit modelsummary
#' @keywords internal
#' @export
msummary <- modelsummary
