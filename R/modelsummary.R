# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'part', 'estimate', 'conf.high', 'conf.low',
'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik',
'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model',
'tmp_grp', 'condition_variable', 'conf_int', 'conf_level'))



#' Model Summary Tables
#'
#' The content of the tables can be altered with the function's arguments, or by
#' calling `options`, as described in the _Details_ section below. The look of
#' the tables can be customized by specifying the `output` argument, and by
#' using functions from one of the supported table customization packages:
#' `kableExtra`, `gt`, `flextable`, `huxtable`.
#'
#' @param models a model or (optionally named) list of models
#' @param output filename or object type (character string)
#' * Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg.
#' * Supported object types: "default", "html", "markdown", "latex", "latex_tabular", "data.frame", "modelsummary_list", "gt", "kableExtra", "huxtable", "flextable".
#' * To change the default output format, type `options(modelsummary_default = "latex")`, where `latex` can be any of the valid object types listed above.
#' * Warning: users should not supply a file name to the `output` argument if they intend to customize the table with external packages.
#' * See the 'Details' section below for more information.
#' @param fmt determines how to format numeric values
#' * integer: the number of digits to keep after the period `format(round(x, fmt), nsmall=fmt)`
#' * character: passed to the `sprintf` function (e.g., '%.3f' keeps 3 digits with trailing zero). See `?sprintf`
#' * function: returns a formatted character string.
#' @param stars to indicate statistical significance
#' * FALSE (default): no significance stars.
#' * TRUE: +=.1, *=.05, **=.01, ***=0.001
#' * Named numeric vector for custom stars such as `c('*' = .1, '+' = .05)`
#' * Note: a legend will not be inserted at the bottom of the table when the `estimate` or `statistic` arguments use "glue strings" with `{stars}`.
#' @param statistic vector of strings or `glue` strings which select uncertainty
#' statistics to report vertically below the estimate. NULL omits all
#' uncertainty statistics.
#' * "conf.int", "std.error", "statistic", "p.value", "conf.low", "conf.high",
#'    or any column name produced by: `get_estimates(model)`
#' * `glue` package strings with braces, such as:
#'   - `"{p.value} [{conf.low}, {conf.high}]"`
#'   - `"Std.Error: {std.error}"`
#' * Note: Parentheses are added automatically unless the string includes `glue` curly braces `{}`.
#' * Note: To report uncertainty statistics \emph{next} to coefficients, you can #'   supply a `glue` string to the `estimate` argument.
#' @param vcov robust standard errors and other manual statistics. The `vcov`
#'   argument accepts six types of input (see the 'Details' and 'Examples'
#'   sections below):
#' * NULL returns the default uncertainty estimates of the model object
#' * string, vector, or (named) list of strings. The strings "classical", "iid" and "constant" are aliases for `NULL`, and they return the model's default uncertainty estimates. The strings "robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "stata", "HAC", "NeweyWest", "Andrews", "panel-corrected", "outer-product", "weave" use variance-covariance matrices computed using functions from the `sandwich` package. The behavior of those functions can (and sometimes *must*) be altered by passing arguments to `sandwich` directly from `modelsummary` through the ellipsis (`...`), but it is safer to define your own custom functions as described in the next bullet.
#' * function or (named) list of functions which return variance-covariance matrices with row and column names equal to the names of your coefficient estimates (e.g., `stats::vcov`, `sandwich::vcovHC`, `function(x) vcovPC(x, cluster="country")`).
#' * formula or (named) list of formulas with the cluster variable(s) on the right-hand side (e.g., ~clusterid).
#' * (named) list of `length(models)` variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#' * a (named) list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. Warning: since this list of vectors can include arbitrary strings or numbers, `modelsummary` cannot automatically calculate p values. The `stars` argument may thus use incorrect significance thresholds when `vcov` is a list of vectors.
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_map character vector. Subset, rename, and reorder coefficients.
#' Coefficients omitted from this vector are omitted from the table. The order
#' of the vector determines the order of the table.  `coef_map` can be a named
#' or an unnamed character vector (see the Examples section below). If
#' `coef_map` is a named vector, its values define the labels that must appear
#' in the table, and its names identify the original term names stored in the
#' model object: `c("hp:mpg"="HPxM/G")`.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table using `grepl(perl=TRUE)`.
#' @param coef_rename named character vector or function which returns a named
#' vector. Values of the vector refer to the variable names that will appear
#' in the table. Names refer to the original term names stored in the model
#' object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' @param gof_map rename, reorder, and omit goodness-of-fit statistics and other
#'   model information. This argument accepts 3 types of values:
#' * NULL (default): the `modelsummary::gof_map` dictionary is used for formatting, and all unknown statistic are included.
#' * data.frame with 3 columns named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples' section below.
#' * list of lists, each of which includes 3 elements named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples section below'.
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `grepl(perl=TRUE)`).
#' @param group a two-sided formula with two or three components which describes
#' how groups of parameters should be displayed. The formula must include both
#' a "term" and a "model" component. In addition, a component can be used to
#' identify groups of parameters (e.g., outcome levels of a multinomial logit
#' model). This group identifier must be the name of a column in the
#' data.frame produced by `get_estimates(model)`.
#' * `term ~ model` displays coefficients as rows and models as columns
#' * `model ~ term` displays models as rows and coefficients as columns
#' * `response + term ~ model` displays response levels and coefficients as rows and models as columns.
#' @param group_map named or unnamed character vector. Subset, rename, and
#' reorder coefficient groups specified in the `group` argument. See `coef_map`.
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See Examples section below.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate string or `glue` string of the estimate to display (or a
#' vector with one string per model). Valid entries include any column name of
#' the data.frame produced by `get_estimates(model)`. Examples:
#' * `"estimate"`
#' * `"{estimate} ({std.error}){stars}"`
#' * `"{estimate} [{conf.low}, {conf.high}]"`
#' @param align A character string of length equal to the number of columns in
#' the table.  "lcr" means that the first column will be left-aligned, the 2nd
#' column center-aligned, and the 3rd column right-aligned.
#' @param ... all other arguments are passed through to the extractor and
#' table-making functions. This allows users to pass arguments directly to
#' `modelsummary` in order to affect the behavior of other functions behind
#' the scenes. Examples include:
#' * `broom::tidy(exponentiate=TRUE)` to exponentiate logistic regression
#' * `kableExtra::kbl(escape=FALSE)` to avoid escaping math characters in `kableExtra` tables.
#' * `performance::model_performance(metrics="RMSE")` to select goodness-of-fit statistics to extract using the `performance` package (must have set `options(modelsummary_get="easystats")` first).
#' @return a regression table in a format determined by the `output` argument.
#' @importFrom generics glance tidy
#' @details
#'
#' `options`
#'
#' `modelsummary` supports 4 table-making packages: `kableExtra`, `gt`,
#' `flextable`, and `huxtable`. Some of these packages have overlapping
#' functionalities. For example, 3 of those packages can export to LaTeX. To
#' change the default backend used for a specific file format, you can use
#' the `options` function:
#'
#' `options(modelsummary_html = 'kableExtra')`
#' `options(modelsummary_latex = 'gt')`
#' `options(modelsummary_word = 'huxtable')`
#' `options(modelsummary_png = 'gt')`
#'
#' `modelsummary` can use two sets of packages to extract information from
#' statistical models: `broom` and the `easystats` family (`performance` and
#' `parameters`). By default, it uses `broom` first and `easystats` as a
#' fallback if `broom` fails. You can change the order of priorities
#' or include goodness-of-fit extracted by *both* packages by setting:
#'
#' `options(modelsummary_get = "broom")`
#' `options(modelsummary_get = "easystats")`
#' `options(modelsummary_get = "all")`
#'
#'
#' `output` argument:
#'
#' The `modelsummary_list` output type is a lightweight representation of the
#' model results. The `modelsummary` function can export to this format by
#' setting the `output` argument, and it can accept objects of this format
#' as input models to create a table. This can be useful to save raw
#' results, in order to print a table later, without having to save and
#' extract from the entire model object. Note that the confidence intervals
#' are only stored in a `modelsummary_list` if explicitly requested:
#'
#' `backup <- modelsummary(models, output = "modelsummary_list"`
#' `                       statistic = "conf.int")`
#' `modelsummary(backup)`
#'
#' When a file name with a valid extension is supplied to the `output` argument,
#' the table is written immediately to file. If you want to customize your table
#' by post-processing it with an external package, you need to choose a
#' different output format and saving mechanism. Unfortunately, the approach
#' differs from package to package:
#' * `gt`: set `output="gt"`, post-process your table, and use the `gt::gtsave` function.
#' * `kableExtra`: set `output` to your destination format (e.g., "latex", "html", "markdown"), post-process your table, and use `kableExtra::save_kable` function.
#'
#'
#' `vcov` argument:
#'
#' To use a string such as "robust" or "HC0", your model must be supported
#' by the `sandwich` package. This includes objects such as: lm, glm,
#' survreg, coxph, mlogit, polr, hurdle, zeroinfl, and more.
#'
#' NULL, "classical", "iid", and "constant" are aliases which do not modify
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
#'               c("(Intercept)"="", "Height"="!", "Volume"="!!")))
#'
#' # vcov with custom names
#' modelsummary(
#'   models,
#'   vcov = list("Stata Corp" = "stata",
#'               "Newey Lewis & the News" = "NeweyWest"))
#'
#' # coef_rename
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # coef_map
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#' modelsummary(models, coef_map = c('Volume', 'Height'))
#'
#' # title
#' modelsummary(models, title = 'This is the title')
#'
#' # title with LaTeX label (for numbering and referencing)
#' modelsummary(models, title = 'This is the title \\label{tab:description}')
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
  group       = term ~ model,
  group_map   = NULL,
  add_rows    = NULL,
  align       = NULL,
  notes       = NULL,
  title       = NULL,
  ...) {



  # sanitation
  sanity_ellipsis(vcov, ...)        # before sanitize_vcov
  models <- sanitize_models(models) # before sanitize_vcov
  vcov <- sanitize_vcov(vcov, length(models), ...)
  number_of_models <- max(length(models), length(vcov))
  estimate <- sanitize_estimate(estimate, number_of_models)
  group <- sanitize_group(group)
  sanity_group_map(group_map)
  sanity_statistic(statistic)
  sanity_conf_level(conf_level)
  sanity_coef(coef_map, coef_rename, coef_omit)
  # no gof should be OK
  ## sanity_gof_map(gof_map, gof_omit)
  sanity_stars(stars)
  sanity_fmt(fmt)

  sanity_output(output)
  output_format <- sanitize_output(output)$output_format

  # confidence intervals are expensive
  if (!any(grepl("conf", c(estimate, statistic)))) {
    conf_level <- NULL
  }

  # model names dictionary: use unique names for manipulation
  if (is.null(names(models))) {
    model_names <- paste("Model", 1:number_of_models)
  } else {
    model_names <- names(models)
  }
  model_names <- pad(model_names)

  #######################
  #  modelsummary_list  #
  #######################
  msl <- get_list_of_modelsummary_lists(models = models,
                                        conf_level = conf_level,
                                        vcov = vcov,
                                        ...)
  names(msl) <- model_names



  if (output_format == "modelsummary_list") {
    if (length(msl) == 1) {
      return(msl[[1]])
    } else {
      return(msl)
    }
  }


  ###############
  #  estimates  #
  ###############
  est <- list()
  for (i in seq_along(msl)) {

    tmp <- format_estimates(
      est        = msl[[i]]$tidy,
      fmt        = fmt,
      estimate   = estimate[[i]],
      statistic  = statistic,
      vcov       = vcov[[i]],
      conf_level = conf_level,
      stars      = stars,
      group_name = group$group_name,
      ...)

    # before merging to collapse
    tmp <- map_omit_rename_estimates(
        tmp,
        coef_rename = coef_rename,
        coef_map = coef_map,
        coef_omit = coef_omit,
        group_map = group_map)

    colnames(tmp)[4] <- model_names[i]

    est[[model_names[i]]] <- tmp

  }


  term_order <- unique(unlist(lapply(est, function(x) x$term)))
  group_order <- unique(unlist(lapply(est, function(x) x$group)))

  f <- function(x, y) merge(x, y, all = TRUE, sort = FALSE,
                            by = c("group", "term", "statistic"))
  est <- Reduce(f, est)

  est <- group_reshape(est, group$lhs, group$rhs, group$group_name)

  # distinguish between estimates and gof (first column for tests)
  est$part <- "estimates"
  est <- est[, unique(c("part", names(est)))]

  # empty cells (needed for factor sorting
  est[is.na(est)] <- ""

  # sort rows using factor trick
  if ("term" %in% colnames(est)) {
    if (!is.null(coef_map)) {
        term_order <- coef_map
        est$term <- factor(est$term, unique(term_order))
    } else {
        est$term <- factor(est$term, unique(term_order))
    }

    if (!is.null(group_map)) {
        group_order <- group_map
        est$group <- factor(est$term, group_order)
    } else {
        est$group <- factor(est$group, unique(est$group))
    }

  } else if ("model" %in% colnames(est)) {
    est$model <- factor(est$model, model_names)
  }

  est <- est[do.call(order, as.list(est)), ]

  # character for binding
  for (col in c("term", "group", "model")) {
    if (col %in% colnames(est)) {
      est[[col]] <- as.character(est[[col]])
    }
  }

  # make sure there are no duplicate estimate names *within* a single model.
  # this cannot be in input sanity checks. idx paste allows multiple statistics.
  if (is.null(group$group_name) && "term" %in% group$lhs) {
    idx <- paste(est$term, est$statistic)
    if (anyDuplicated(idx) > 1) {
      warning('The table includes duplicate term names. This can happen when `coef_map` or `coef_rename` are misused. This can also happen when a model produces "grouped" terms, such as in multinomial logit or gamlss models. You may want to call `get_estimates(model)` to see how estimates are labelled internally, and use the `group` argument of the `modelsummary` function.')
    }
  }


  #####################
  #  goodness-of-fit  #
  #####################
  gof <- list()
  for (i in seq_along(msl)) {
    if (is.data.frame(msl[[i]]$glance)) {
      gof[[i]] <- format_gof(msl[[i]]$glance,
                             fmt = fmt,
                             gof_map = gof_map,
                             ...)
      colnames(gof[[i]])[2] <- model_names[i]
    } else {
      gof[[i]] <- NULL
    }
  }


  f <- function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = "term")
  gof <- Reduce(f, gof)

  gof <- map_omit_gof(gof, gof_omit, gof_map)

  # combine estimates and gof
  if (is.data.frame(gof) &&
      nrow(gof) > 0 &&
      all(colnames(gof) %in% colnames(est))) {
    tab <- bind_rows(est, gof)
  } else {
    tab <- est
  }



  ##################
  #  output table  #
  ##################

  # empty cells
  tab[is.na(tab)] <- ''

  # interaction : becomes ×
  if (is.null(coef_map) &&
      "term" %in% colnames(tab) &&
      output_format != 'rtf') {
    idx <- tab$part != 'gof'
    tab$term <- ifelse(idx, gsub(':', ' \u00d7 ', tab$term), tab$term)
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
  if (isTRUE(stars)) {
    rlang::warn(
      message = "In version 0.8.0 of the `modelsummary` package, the default significance markers produced by the `stars=TRUE` argument were changed to be consistent with R's defaults.",
      .frequency = "once",
      .frequency_id = "stars_true_consistency")
  }
  if (!isFALSE(stars) && !any(grepl("\\{stars\\}", c(estimate, statistic)))) {
    stars_note <- make_stars_note(stars)
    if (is.null(notes)) {
      notes <- stars_note
    } else {
      notes <- c(stars_note, notes)
    }
  }

  # data.frame output keeps redundant info
  if (output_format != "dataframe") {

    tab <- redundant_labels(tab, "model")
    tab <- redundant_labels(tab, "group")
    tab <- redundant_labels(tab, "term")

    # after label redundancy
    tab$statistic <- tab$part <- NULL

    # HACK: arbitrary spaces to avoid name conflict
    if ("term" %in% colnames(tab)) colnames(tab)[colnames(tab) == "term"]   <- "       "
    if ("model" %in% colnames(tab)) colnames(tab)[colnames(tab) == "model"] <- "         "

  }

  # only show group label if it is a row-property (lhs of the group formula)
  tmp <- setdiff(group$lhs, c("model", "term"))
  if (length(tmp) == 0) {
    tab$group <- NULL
  } else if (output_format != "dataframe") {
    colnames(tab)[colnames(tab) == "group"] <- "        "
  }

  # align
  if (is.null(align)) {
    if (!is.null(group) && length(group$lhs) == 2) {
      align <- paste0("ll", strrep("c", ncol(tab) - 2))
    } else {
      align <- paste0("l", strrep("c", ncol(tab) - 1))
    }
  }


  # remove "empty" confidence intervals or standard errors (HACK)
  for (i in seq_along(tab)) {
    tab[[i]] <- gsub("\\[,\\s*\\]|\\(\\s*\\)", "", tab[[i]])
  }


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



#' rename and reorder estimates from a *single* model
#' (before merging to collapse)
#'
#' @keywords internal
map_omit_rename_estimates <- function(estimates,
                             coef_rename,
                             coef_map,
                             coef_omit,
                             group_map) {


    # coef_omit
    if (!is.null(coef_omit)) {
        idx <- !grepl(coef_omit, estimates$term, perl = TRUE)
        estimates <- estimates[idx, , drop = FALSE]
    }

    # coef_rename
    if (!is.null(coef_rename)) {
        if (is.character(coef_rename)) {
            dict <- coef_rename
        } else if (is.function(coef_rename)) {
            dict <- stats::setNames(coef_rename(estimates$term), estimates$term)
        }
        estimates$term <- replace_dict(estimates$term, dict)
    }


    # coef_map
    if (!is.null(coef_map)) {
        if (is.null(names(coef_map))) {
            coef_map <- stats::setNames(coef_map, coef_map)
        }
        estimates <- estimates[estimates$term %in% names(coef_map), , drop = FALSE]
        estimates$term <- replace_dict(estimates$term, coef_map)
    }


    # group_map
    if (!is.null(group_map)) {
        if (is.null(names(group_map))) {
            group_map <- stats::setNames(group_map, group_map)
        }
        estimates <- estimates[estimates$group %in% names(group_map), , drop = FALSE]
        estimates$group <- replace_dict(estimates$group, group_map)
    }


    return(estimates)
}


#' Internal function to subset, rename and re-order gof statistics
#'
#' @keywords internal
map_omit_gof <- function(gof, gof_omit, gof_map) {

  if (is.null(gof) ||
      is.data.frame(gof) && nrow(gof) == 0) {
    return(gof)
  }

  # row identifier
  gof$part <- "gof"

  gof <- gof[, unique(c("part", "term", names(gof)))]

  # omit
  if (!is.null(gof_omit)) {
    idx <- !grepl(gof_omit, gof$term, perl = TRUE)
    gof <- gof[idx, , drop = FALSE]
  }

  # map
  if (is.null(gof_map)) {
    # assign here and not in the function definition because we use NULL to
    # figure out if F-stat should be included by default for lm models.
    gm_list <- get("gof_map", envir = loadNamespace("modelsummary"))
    gm_list <- lapply(seq_len(nrow(gm_list)), function(i) gm_list[i, ])
  } else if (inherits(gof_map, "data.frame")) {
    gm_list <- lapply(seq_len(nrow(gof_map)), function(i) gof_map[i, ])
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

  # important for modelsummary_get="all"
  gof <- unique(gof)

  return(gof)
}


#' internal function to reshape grouped estimates
#'
#' @keywords internal
#' @noRd
group_reshape <- function(estimates, lhs, rhs, group_name) {

    lhs[lhs == group_name] <- "group"
    rhs[rhs == group_name] <- "group"

    # term ~ model (standard)
    if (is.null(lhs) ||
        (length(lhs) == 1 && lhs == "term" &&
         length(rhs) == 1 && rhs == "model")) {
      return(estimates)

    # model ~ term
    } else if (length(lhs) == 1 && lhs == "model" &&
        length(rhs) == 1 && rhs == "term") {
      out <- tidyr::pivot_longer(estimates,
                                 cols = -c("group", "term", "statistic"),
                                 names_to = "model")
      out <- tidyr::pivot_wider(out, names_from = "term")

      # order matters for sorting
      out <- out[, unique(c("group", "model", "statistic", colnames(out)))]

    # term + group ~ model
    } else if (all(c("term", "group") %in% lhs)) {
        idx <- unique(c(lhs, colnames(estimates)))
        out <- estimates[, idx, drop = FALSE]

    } else if (all(c("term", "model") %in% lhs)) {
        out <- estimates
        out <- tidyr::pivot_longer(
            out,
            cols = !tidyselect::any_of(c("part", "group", "term", "statistic")),
            names_to = "model")
        out <- tidyr::pivot_wider(
            out,
            names_from = "group",
            values_from = "value",
            values_fill = "")
        idx <- unique(c(lhs, colnames(out)))
        out <- out[, idx, drop = FALSE]

    } else if (all(c("group", "model") %in% rhs)) {
        out <- estimates
        out <- tidyr::pivot_longer(out,
                                   cols = !tidyselect::any_of(c("part", "group", "term", "statistic")),
                                   names_to = "model")
        out$idx_col <- paste(out[[rhs[1]]], "/", out[[rhs[2]]])
        out$model <- out$group <- NULL
        out <- tidyr::pivot_wider(out,
                                  names_from = "idx_col",
                                  values_from = "value",
                                  values_fill = "")
    }

    out[out == "NA"] <- ""
    out[is.na(out)] <- ""

    # empty columns
    idx <- sapply(out, function(x) !all(x == ""))
    out <- out[, idx, drop = FALSE]

    # empty rows
    idx <- setdiff(colnames(out), c("part", "term", "statistic", "model"))
    tmp <- out[, idx, drop = FALSE]
    idx <- apply(tmp, 1, function(x) !all(x == ""))
    out <- out[idx, ]

    # make sure there is a group column for merging in `modelsummary`
    if (!"group" %in% colnames(out)) {
      out$group <- "group"
    }

    return(out)
}


get_list_of_modelsummary_lists <- function(models, conf_level, vcov, ...) {

    number_of_models <- max(length(models), length(vcov))

    vcov_type <- get_vcov_type(vcov)

    vcov_names <- names(vcov)


    # warning for models with hard-coded non-IID vcov
    hardcoded <- c("fixest", "lm_robust")
    flag_vcov <- NULL

    for (i in 1:number_of_models) {
        # trust users when they specify vcov names
        if (is.null(vcov_names[[i]]) || vcov_names[[i]] == "") {
            j <- ifelse(length(models) == 1, 1, i)
            if (is.character(vcov_type[[i]]) &&
                tolower(vcov_type[[i]]) %in% c("iid", "classical", "constant") &&
                length(intersect(hardcoded, class(models[[j]])) > 0)) {
                flag_vcov <- i
            }
        }
    }

    if (!is.null(flag_vcov)) {
        j <- ifelse(length(models) == 1, 1, flag_vcov)
        warning(sprintf('When the `vcov` argument is set to "iid", "classical", or "constant", `modelsummary` extracts the default variance-covariance matrix from the model object. For objects of class `%s`, the default vcov is not always IID. Please make sure that the standard error label matches the numeric results in the table. Note that the `vcov` argument accepts a named list for users who want to customize the standard error labels in their regression tables.', class(models[[j]])[1]))
    }

    # extract
    out <- list()

    for (i in 1:number_of_models) {
        # recycling when 1 model and many vcov
        j <- ifelse(length(models) == 1, 1, i)

        gla <- get_gof(models[[j]], vcov_type[[i]], ...)
        tid <- get_estimates(models[[j]], conf_level = conf_level, vcov = vcov[[i]], ...)

        out[[i]] <- list("tidy" = tid, "glance" = gla)
        class(out[[i]]) <- "modelsummary_list"
    }

    return(out)
}


redundant_labels <- function(dat, column) {
    if (!column %in% colnames(dat)) {
        return(dat)
    }
    for (i in nrow(dat):2) {
        if (dat$part[i] == "estimates" &&
        dat[[column]][i - 1] == dat[[column]][i]) {
        dat[[column]][i] <- ""
        }
    }
    return(dat)
}

#' `msummary()` is a shortcut to `modelsummary()`
#'
#' @inherit modelsummary
#' @keywords internal
#' @export
msummary <- modelsummary
