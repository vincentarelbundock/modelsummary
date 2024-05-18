# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 wickham says "globalVariables is a hideous hack and I will never use it"
# 2014 wickham updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'part', 'estimate', 'conf.high', 'conf.low',
'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik',
'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model',
'tmp_grp', 'condition_variable', 'conf_int', 'conf_level', '..idx', 'Internal Data List', 'Variable', 'variable'))



#' Model Summary Tables
#'
#' Create beautiful and customizable tables to summarize several statistical
#' models side-by-side. This function supports dozens of statistical models,
#' and it can produce tables in HTML, LaTeX, Word, Markdown, Typst, PDF, PowerPoint,
#' Excel, RTF, JPG, or PNG. The appearance of the tables can be customized
#' extensively by specifying the `output` argument, and by using functions from
#' one of the supported table customization packages: `tinytable`, `kableExtra`, `gt`,
#' `flextable`, `huxtable`, `DT`. For more information, see the Details and Examples
#' sections below, and the vignettes on the `modelsummary` website:
#' https://modelsummary.com/
#' * [The `modelsummary` Vignette includes dozens of examples of tables with extensive customizations.](https://modelsummary.com/articles/modelsummary.html)
#' * [The Appearance Vignette shows how to modify the look of tables.](https://modelsummary.com/articles/appearance.html)
#'
#' @template citation
#'
#' @template modelsummary_details
#'
#' @template options
#'
#' @template modelsummary_parallel
#'
#' @param models a model, (named) list of models, or nested list of models.
#' * Single model: `modelsummary(model)`
#' * Unnamed list of models: `modelsummary(list(model1, model2))`
#'   - Models are labelled automatically. The default label style can be altered by setting a global option. See below.
#' * Named list of models: `modelsummary(list("A"=model1, "B"=model2))`
#'   - Models are labelled using the list names.
#' * Nested list of models: 
#'   - When using the `shape` argument with "rbind", "rcollapse", or "cbind" values, `models` can be a nested list of models to display "panels" or "stacks" of regression models. See the `shape` argument documentation and examples below.
#' @param output filename or object type (character string)
#' * Supported filename extensions: .docx, .html, .tex, .md, .txt, .csv, .xlsx, .png, .jpg
#' * Supported object types: "default", "html", "markdown", "latex", "latex_tabular", "typst", "data.frame", "tinytable", "gt", "kableExtra", "huxtable", "flextable", "DT", "jupyter". The "modelsummary_list" value produces a lightweight object which can be saved and fed back to the `modelsummary` function.
#' * The "default" output format can be set to "tinytable", "kableExtra", "gt", "flextable", "huxtable", "DT", or "markdown"
#'   - If the user does not choose a default value, the packages listed above are tried in sequence.
#'   - Session-specific configuration: `options("modelsummary_factory_default" = "gt")`
#'   - Persistent configuration: `config_modelsummary(output = "markdown")`
#' * Warning: Users should not supply a file name to the `output` argument if they intend to customize the table with external packages. See the 'Details' section.
#' * LaTeX compilation requires the `booktabs` and `siunitx` packages, but `siunitx` can be disabled or replaced with global options. See the 'Details' section.
#' @param fmt how to format numeric values: integer, user-supplied function, or `modelsummary` function.
#' * Integer: Number of decimal digits
#' * User-supplied functions:
#'   - Any function which accepts a numeric vector and returns a character vector of the same length.
#' * `modelsummary` functions:
#'   - `fmt = fmt_significant(2)`: Two significant digits (at the term-level)
#'   - `fmt = fmt_decimal(digits = 2, pdigits = 3)`: Decimal digits for estimate and p values
#'   - `fmt = fmt_sprintf("%.3f")`: See `?sprintf`
#'   - `fmt = fmt_term("(Intercept)" = 1, "X" = 2)`: Format terms differently
#'   - `fmt = fmt_statistic("estimate" = 1, "r.squared" = 6)`: Format statistics differently.
#'   - `fmt = fmt_identity()`: unformatted raw values
#' * string:
#' * Note on LaTeX output: To ensure proper typography, all numeric entries are enclosed in the `\num{}` command, which requires the `siunitx` package to be loaded in the LaTeX preamble. This behavior can be altered with global options. See the 'Details' section.
#' @param stars to indicate statistical significance
#' * FALSE (default): no significance stars.
#' * TRUE: +=.1, *=.05, **=.01, ***=0.001
#' * Named numeric vector for custom stars such as `c('*' = .1, '+' = .05)`
#' * Note: a legend will not be inserted at the bottom of the table when the `estimate` or `statistic` arguments use "glue strings" with `{stars}`.
#' @param statistic vector of strings or `glue` strings which select uncertainty statistics to report vertically below the estimate. NULL omits all uncertainty statistics.
#' * "conf.int", "std.error", "statistic", "p.value", "conf.low", "conf.high", or any column name produced by `get_estimates(model)`
#' * `glue` package strings with braces, with or without R functions, such as:
#'   - `"{p.value} [{conf.low}, {conf.high}]"`
#'   - `"Std.Error: {std.error}"`
#'   - `"{exp(estimate) * std.error}"`
#'   - Numbers are automatically rounded and converted to strings. To apply functions to their numeric values, as in the last `glue` example, users must set `fmt=NULL`.
#'   - Parentheses are added automatically unless the string includes `glue` curly braces `{}`.
#' * Notes: 
#'   - The names of the `statistic` are used a column names when using the `shape` argument to display statistics as columns: 
#'      - `statistic=c("p"="p.value", "["="conf.low", "]"="conf.high")`
#'   - Some statistics are not supported for all models. See column names in `get_estimates(model)`, and visit the website to learn how to add custom statistics.
#' @param vcov robust standard errors and other manual statistics. The `vcov`
#'   argument accepts six types of input (see the 'Details' and 'Examples'
#'   sections below):
#' * NULL returns the default uncertainty estimates of the model object
#' * string, vector, or (named) list of strings. "iid", "classical", and "constant" are aliases for `NULL`, which returns the model's default uncertainty estimates. The strings "HC", "HC0", "HC1" (alias: "stata"), "HC2", "HC3" (alias: "robust"), "HC4", "HC4m", "HC5", "HAC", "NeweyWest", "Andrews", "panel-corrected", "outer-product", and "weave" use variance-covariance matrices computed using functions from the `sandwich` package, or equivalent method. "BS", "bootstrap", "residual", "mammen", "webb", "xy", "wild" use the `sandwich::vcovBS()`. The behavior of those functions can (and sometimes *must*) be altered by passing arguments to `sandwich` directly from `modelsummary` through the ellipsis (`...`), but it is safer to define your own custom functions as described in the next bullet.
#'
#' * function or (named) list of functions which return variance-covariance matrices with row and column names equal to the names of your coefficient estimates (e.g., `stats::vcov`, `sandwich::vcovHC`, `function(x) vcovPC(x, cluster="country")`).
#' * formula or (named) list of formulas with the cluster variable(s) on the right-hand side (e.g., ~clusterid).
#' * named list of `length(models)` variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#' * a named list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. Warning: since this list of vectors can include arbitrary strings or numbers, `modelsummary` cannot automatically calculate p values. The `stars` argument may thus use incorrect significance thresholds when `vcov` is a list of vectors.

#' @param conf_level numeric value between 0 and 1. confidence level to use for
#' confidence intervals. Setting this argument to `NULL` does not extract
#' confidence intervals, which can be faster for some models.
#' @param exponentiate TRUE, FALSE, or logical vector of length equal to the
#' number of models. If TRUE, the `estimate`, `conf.low`, and `conf.high`
#' statistics are exponentiated, and the `std.error` is transformed to
#' `exp(estimate)*std.error`.
#' @param coef_map character vector. Subset, rename, and reorder coefficients.
#' Coefficients omitted from this vector are omitted from the table. The order
#' of the vector determines the order of the table. `coef_map` can be a named
#' or an unnamed character vector. If `coef_map` is a named vector, its values
#' define the labels that must appear in the table, and its names identify the
#' original term names stored in the model object: `c("hp:mpg"="HPxM/G")`. See
#' Examples section below.
#' @param coef_omit integer vector or regular expression to identify which coefficients to omit (or keep) from the table. Positive integers determine which coefficients to omit. Negative integers determine which coefficients to keep. A regular expression can be used to omit coefficients, and perl-compatible "negative lookaheads" can be used to specify which coefficients to *keep* in the table. Examples:
#' * c(2, 3, 5): omits the second, third, and fifth coefficients.
#' * c(-2, -3, -5): negative values keep the second, third, and fifth coefficients.
#' * `"ei"`: omit coefficients matching the "ei" substring.
#' * `"^Volume$"`: omit the "Volume" coefficient.
#' * `"ei|rc"`: omit coefficients matching either the "ei" or the "rc" substrings.
#' * `"^(?!Vol)"`: keep coefficients starting with "Vol" (inverse match using a negative lookahead).
#' * `"^(?!.*ei)"`: keep coefficients matching the "ei" substring.
#' * `"^(?!.*ei|.*pt)"`: keep coefficients matching either the "ei" or the "pt" substrings.
#' * See the Examples section below for complete code.
#' @param coef_rename logical, named or unnamed character vector, or function
#' * Logical: TRUE renames variables based on the "label" attribute of each column. See the Example section below.
#' * Unnamed character vector of length equal to the number of coefficients in the final table, after `coef_omit` is applied.
#' * Named character vector: Values refer to the variable names that will appear in the table. Names refer to the original term names stored in the model object. Ex: c("hp:mpg"="hp X mpg")
#' * Function: Accepts a character vector of the model's term names and returns a named vector like the one described above. The `modelsummary` package supplies a `coef_rename()` function which can do common cleaning tasks: `modelsummary(model, coef_rename = coef_rename)`
#' @param gof_map rename, reorder, and omit goodness-of-fit statistics and other
#'   model information. This argument accepts 4 types of values:
#' * NULL (default): the `modelsummary::gof_map` dictionary is used for formatting, and all unknown statistic are included.
#' * character vector: "all", "none", or a vector of statistics such as `c("rmse", "nobs", "r.squared")`. Elements correspond to colnames in the data.frame produced by `get_gof(model)`. The `modelsummary::gof_map` default dictionary is used to format and rename statistics.
#' * NA: excludes all statistics from the bottom part of the table.
#' * data.frame with 3 columns named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples' section below. The `fmt` column in this data frame only accepts integers. For more flexibility, use a list of lists, as described in the next bullet.
#' * list of lists, each of which includes 3 elements named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples section below'.
#' @param gof_omit string regular expression (perl-compatible) used to determine which statistics to omit from the bottom section of the table. A "negative lookahead" can be used to specify which statistics to *keep* in the table. Examples:
#' * `"IC"`: omit statistics matching the "IC" substring.
#' * `"BIC|AIC"`: omit statistics matching the "AIC" or "BIC" substrings.
#' * `"^(?!.*IC)"`: keep statistics matching the "IC" substring.
#' @param gof_function function which accepts a model object in the `model` argument and returns a 1-row `data.frame` with one custom goodness-of-fit statistic per column.
#' @param group_map named or unnamed character vector. Subset, rename, and
#' reorder coefficient groups specified a grouping variable specified in the
#' `shape` argument formula. This argument behaves like `coef_map`.
#' @param shape `NULL`, formula, or string which determines the shape of a table.
#' * `NULL`: Default shape with terms in rows and models in columns.
#' * Formula: The left side determines what appears on rows, and the right side determines what appears on columns. The formula can include one or more group identifier(s) to display related terms together, which can be useful for models with multivariate outcomes or grouped coefficients (See examples section below). The group identifier(s) must be column names produced by: `get_estimates(model)`. The group identifier(s) can be combined with the term identifier in a single column by using the colon to represent an interaction. If an incomplete formula is supplied (e.g., `~statistic`), `modelsummary` tries to complete it automatically. Goodness-of-fit statistics are only appended to the bottom of the table when `model` is on the right hand side of the formula (i.e., columns). Potential `shape` values include:
#'   - `term + statistic ~ model`: default
#'   - `term ~ model + statistic`: statistics in separate columns
#'   - `model + statistic ~ term`: models in rows and terms in columns
#'   - `term + response + statistic ~ model`: term and group id in separate columns
#'   - `term : response + statistic ~ model`: term and group id in a single column
#'   - `term ~ response`
#' * String: "cbind", "rbind", "rcollapse"
#'   - "cbind": side-by-side models with autmoatic spanning column headers to group models (`tinytable` only feature).
#'   - "rbind" or "rcollapse": "panels" or "stacks" of regression models.
#'   -  the `models` argument must be a (potentially named) nested list of models.
#'     + Unnamed nested list with 2 panels: `list(list(model1, model2), list(model3, model4))`
#'     + Named nested list with 2 panels: `list("Panel A" = list(model1, model2), "Panel B" = list(model3, model4))`
#'     + Named panels and named models: `list("Panel A" = list("(I)" = model1, "(II)" = model2), "Panel B" = list("(I)" = model3, "(II)" = model4))`
#'   - "rbind": Bind the rows of independent regression tables
#'   - "rcollapse": Bind the rows of regression tables and create a panel at the bottom where we "collapse" goodness-of-fit statistics which are identical across models.
#' @param add_columns a data.frame (or tibble) with the same number of rows as
#' #' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the columns positions.
#' See Examples section below.
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See Examples section below.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate a single string or a character vector of length equal to the
#' number of models. Valid entries include any column name of
#' the data.frame produced by `get_estimates(model)`, and strings with curly braces compatible with the `glue` package format. Examples:
#' * `"estimate"`
#' * `"{estimate} ({std.error}){stars}"`
#' * `"{estimate} [{conf.low}, {conf.high}]"`
#' @param align A string with a number of characters equal to the number of columns in
#' the table (e.g., `align = "lcc"`).  Valid characters: l, c, r, d.
#' * "l": left-aligned column
#' * "c": centered column
#' * "r": right-aligned column
#' * "d": dot-aligned column. For LaTeX/PDF output, this option requires at least version 3.0.25 of the siunitx LaTeX package. See the LaTeX preamble help section below for commands to insert in your LaTeX preamble.
#' @param escape boolean TRUE escapes or substitutes LaTeX/HTML characters which could
#' prevent the file from compiling/displaying. `TRUE` escapes all cells, captions, and notes. Users can have more fine-grained control by setting `escape=FALSE` and using an external command such as: `modelsummary(model, "latex") |> tinytable::format_tt(tab, j=1:5, escape=TRUE)`
#' @param ... all other arguments are passed through to three functions. See the documentation of these functions for lists of available arguments.
#' + [parameters::model_parameters] extracts parameter estimates. Available arguments depend on model type, but include:
#'     - `standardize`, `include_reference`, `centrality`, `dispersion`, `test`, `ci_method`, `prior`, `diagnostic`, `rope_range`, `power`, `cluster`, etc.
#' + [performance::model_performance] extracts goodness-of-fit statistics. Available arguments depend on model type, but include:
#'     - `metrics`, `estimator`, etc.
#' + [tinytable::tt], [kableExtra::kbl] or [gt::gt] draw tables, depending on the value of the `output` argument.
#' @return a regression table in a format determined by the `output` argument.
#' @importFrom generics glance tidy
#' @examplesIf isTRUE(Sys.getenv("R_NOT_CRAN") == 'true')
#' # The `modelsummary` website includes \emph{many} examples and tutorials:
#' # https://modelsummary.com
#'
#' library(modelsummary)
#' 
#' # load data and estimate models
#' utils::data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#' 
#' # simple table
#' modelsummary(models)
#' 
#' # statistic
#' modelsummary(models, statistic = NULL)
#' 
#' modelsummary(models, statistic = 'p.value')
#' 
#' modelsummary(models, statistic = 'statistic')
#' 
#' modelsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' 
#' modelsummary(models, statistic = c("t = {statistic}",
#'                                    "se = {std.error}",
#'                                    "conf.int"))
#' 
#' # estimate
#' modelsummary(models,
#'   statistic = NULL,
#'   estimate = "{estimate} [{conf.low}, {conf.high}]")
#' 
#' modelsummary(models,
#'   estimate = c("{estimate}{stars}",
#'                "{estimate} ({std.error})"))
#' 
#' # vcov
#' modelsummary(models, vcov = "robust")
#' 
#' modelsummary(models, vcov = list("classical", "stata"))
#' 
#' modelsummary(models, vcov = sandwich::vcovHC)
#' 
#' modelsummary(models,
#'   vcov = list(stats::vcov, sandwich::vcovHC))
#' 
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
#' # fmt
#' mod <- lm(mpg ~ hp + drat + qsec, data = mtcars)
#' 
#' modelsummary(mod, fmt = 3)
#' 
#' modelsummary(mod, fmt = fmt_significant(3))
#' 
#' modelsummary(mod, fmt = NULL)
#' 
#' modelsummary(mod, fmt = fmt_decimal(4))
#' 
#' modelsummary(mod, fmt = fmt_sprintf("%.5f"))
#' 
#' modelsummary(mod, fmt = fmt_statistic(estimate = 4, conf.int = 1), statistic = "conf.int")
#' 
#' modelsummary(mod, fmt = fmt_term(hp = 4, drat = 1, default = 2))
#' 
#' m <- lm(mpg ~ I(hp * 1000) + drat, data = mtcars)
#' f <- function(x) format(x, digits = 3, nsmall = 2, scientific = FALSE, trim = TRUE)
#' modelsummary(m, fmt = f, gof_map = NA)
#' 
#' # coef_rename
#' modelsummary(models, coef_rename = c('Volume' = 'Large', 'Height' = 'Tall'))
#' 
#' modelsummary(models, coef_rename = toupper)
#' 
#' modelsummary(models, coef_rename = coef_rename)
#' 
#' # coef_rename = TRUE for variable labels
#' datlab <- mtcars
#' datlab$cyl <- factor(datlab$cyl)
#' attr(datlab$hp, "label") <- "Horsepower"
#' attr(datlab$cyl, "label") <- "Cylinders"
#' modlab <- lm(mpg ~ hp * drat + cyl, data = datlab)
#' modelsummary(modlab, coef_rename = TRUE)
#' 
#' # coef_rename: unnamed vector of length equal to the number of terms in the final table
#' m <- lm(hp ~ mpg + factor(cyl), data = mtcars)
#' modelsummary(m, coef_omit = -(3:4), coef_rename = c("Cyl 6", "Cyl 8"))
#' 
#' # coef_map
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#' 
#' modelsummary(models, coef_map = c('Volume', 'Height'))
#' 
#' # coef_omit: omit the first and second coefficients
#' modelsummary(models, coef_omit = 1:2)
#' 
#' # coef_omit: omit coefficients matching one substring
#' modelsummary(models, coef_omit = "ei", gof_omit = ".*")
#' 
#' # coef_omit: omit a specific coefficient
#' modelsummary(models, coef_omit = "^Volume$", gof_omit = ".*")
#' 
#' # coef_omit: omit coefficients matching either one of two substring
#' #modelsummary(models, coef_omit = "ei|rc", gof_omit = ".*")
#' 
#' # coef_omit: keep coefficients starting with a substring (using a negative lookahead)
#' #modelsummary(models, coef_omit = "^(?!Vol)", gof_omit = ".*")
#' 
#' # coef_omit: keep coefficients matching a substring
#' modelsummary(models, coef_omit = "^(?!.*ei|.*pt)", gof_omit = ".*")
#' 
#' # shape: multinomial model
#' library(nnet)
#' multi <- multinom(factor(cyl) ~ mpg + hp, data = mtcars, trace = FALSE) 
#' 
#' # shape: term names and group ids in rows, models in columns
#' modelsummary(multi, shape = response ~ model)
#' 
#' # shape: term names and group ids in rows in a single column
#' modelsummary(multi, shape = term : response ~ model)
#' 
#' # shape: term names in rows and group ids in columns
#' modelsummary(multi, shape = term ~ response:model)
#' 
#' # shape = "rcollapse"
#' panels <- list(
#'     "Panel A: MPG" = list(
#'         "A" = lm(mpg ~ hp, data = mtcars),
#'         "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
#'     "Panel B: Displacement" = list(
#'         "A" = lm(disp ~ hp, data = mtcars),
#'         "C" = lm(disp ~ hp + factor(gear), data = mtcars))
#' )
#' 
#' # shape = "cbind"
#' modelsummary(panels, shape = "cbind")
#' 
#' modelsummary(
#'     panels,
#'     shape = "rbind",
#'     gof_map = c("nobs", "r.squared"))
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
#' # gof_map: tribble
#' library(tibble)
#' gm <- tribble(
#'   ~raw,        ~clean,      ~fmt,
#'   "r.squared", "R Squared", 5)
#' modelsummary(models, gof_map = gm)
#' @export
modelsummary <- function(
  models,
  output      = "default",
  fmt         = 3,
  estimate    = "estimate",
  statistic   = "std.error",
  vcov        = NULL,
  conf_level  = 0.95,
  exponentiate = FALSE,
  stars       = FALSE,
  shape       = term + statistic ~ model,
  coef_map    = NULL,
  coef_omit   = NULL,
  coef_rename = FALSE,
  gof_map     = NULL,
  gof_omit    = NULL,
  gof_function  = NULL,
  group_map   = NULL,
  add_columns = NULL,
  add_rows    = NULL,
  align       = NULL,
  notes       = NULL,
  title       = NULL,
  escape      = TRUE,
  ...) {

  # panel summary shape: dispatch to other function
  checkmate::assert(
    checkmate::check_formula(shape),
    checkmate::check_choice(shape, c("cbind", "rbind", "rcollapse")),
    checkmate::check_null(shape))

  if (isTRUE(checkmate::check_choice(shape, c("rbind", "rcollapse")))) {
    out <- modelsummary_rbind(models,
      output = output,
      fmt = fmt,
      estimate = estimate,
      statistic = statistic,
      vcov = vcov,
      conf_level = conf_level,
      exponentiate = exponentiate,
      stars = stars,
      coef_map = coef_map,
      coef_omit = coef_omit,
      coef_rename = coef_rename,
      gof_map = gof_map,
      gof_omit = gof_omit,
      gof_function = gof_function,
      add_columns = add_columns,
      add_rows = add_rows,
      align = align,
      shape = shape,
      group_map = NULL,
      notes = notes,
      title = title,
      escape = escape,
      ...)
      return(out)
  }

  dots <- list(...)

  ## settings
  if (!settings_equal("function_called", "modelsummary_rbind")) {
    settings_init(settings = list(
      "function_called" = "modelsummary"
    ))
  }


  ## sanity functions validate variables/settings
  ## sanitize functions validate & modify & initialize
  checkmate::assert_string(gof_omit, null.ok = TRUE)
  sanitize_output(output)           # early

  # shape="cbind" only available with `tinytable`
  # before sanitize_models()
  # before sanitize_shape()
  # after sanitize_output()
  tmp <- get_span_cbind(models, shape)
  shape <- tmp$shape
  models <- tmp$models
  span_cbind <- tmp$span_cbind

  # other sanity checks
  sanitize_escape(escape)
  sanity_ellipsis(vcov, ...)        # before sanitize_vcov
  models <- sanitize_models(models, ...) # before sanitize_vcov
  vcov <- sanitize_vcov(vcov, models, ...)
  number_of_models <- max(length(models), length(vcov))
  estimate <- sanitize_estimate(estimate, number_of_models)
  exponentiate <- sanitize_exponentiate(exponentiate, number_of_models)
  shape <- sanitize_shape(shape)
  statistic <- sanitize_statistic(statistic, shape, conf_level) # after shape
  gof_map <- sanitize_gof_map(gof_map)
  fmt <- sanitize_fmt(fmt, calling_function = "modelsummary")
  sanity_group_map(group_map)
  conf_level <- sanitize_conf_level(conf_level, estimate, statistic)
  sanity_coef(coef_map, coef_rename, coef_omit)
  sanity_stars(stars)
  sanity_align(align, estimate = estimate, statistic = statistic, stars = stars)
  checkmate::assert_function(gof_function, null.ok = TRUE)


  # confidence intervals are expensive
  if (!any(grepl("conf", c(estimate, statistic)))) {
    conf_level <- NULL
  }

  # model names dictionary: use unique names for manipulation
  modelsummary_model_labels <- getOption("modelsummary_model_labels", default = "(arabic)")
  if (is.null(names(models))) {
    checkmate::assert_choice(
      modelsummary_model_labels,
      choices = c("model", "arabic", "letters", "roman", "(arabic)", "(letters)", "(roman)"))
    if (modelsummary_model_labels == "model") {
      model_names <- paste("Model", 1:number_of_models)
    } else if (grepl("arabic", modelsummary_model_labels)) {
      model_names <- as.character(1:number_of_models)
    } else if (grepl("letters", modelsummary_model_labels)) {
      model_names <- LETTERS[1:number_of_models]
    } else if (grepl("roman", modelsummary_model_labels)) {
      model_names <- as.character(utils::as.roman(1:number_of_models))
    }
    if (grepl("\\(", modelsummary_model_labels)) {
      model_names <- sprintf("(%s)", model_names)
    }

  } else {
    model_names <- names(models)
  }
  model_names <- pad(model_names)

  # kableExtra sometimes converts (1), (2) to list items, which breaks formatting
  # insert think white non-breaking space
  # don't do this now when called from modelsummary_rbind() or there are escape issues
  if (!settings_equal("function_called", "modelsummary_rbind") &&
      all(grepl("^\\(\\d+\\)$", model_names)) &&
      settings_equal("output_format", "kableExtra")) {
    model_names <- paste0("&nbsp;", model_names)
  }



  #######################
  #  modelsummary_list  #
  #######################
  msl <- get_list_of_modelsummary_lists(models = models,
                                        conf_level = conf_level,
                                        vcov = vcov,
                                        gof_map = gof_map, # check if we can skip all gof computation
                                        gof_function = gof_function,
                                        shape = shape,
                                        coef_rename = coef_rename,
                                        ...)
  names(msl) <- model_names


  if (settings_equal("output_format", "modelsummary_list")) {
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
      # enforce single name when multiple estimates in different colums
      estimate_label = names(estimate)[1],
      statistic  = statistic,
      vcov       = vcov[[i]],
      conf_level = conf_level,
      stars      = stars,
      shape      = shape,
      group_name = shape$group_name,
      exponentiate = exponentiate[[i]],
      ...)

    # before merging to collapse
    tmp <- map_estimates(
        tmp,
        coef_rename = coef_rename,
        coef_map = coef_map,
        coef_omit = coef_omit,
        group_map = group_map)

    colnames(tmp)[match("modelsummary_value", colnames(tmp))] <- model_names[i]

    est[[model_names[i]]] <- tmp
  }

  term_order <- unique(unlist(lapply(est, function(x) x$term)))
  statistic_order <- unique(unlist(lapply(est, function(x) x$statistic)))

  bycols <- c(list(c(shape$group_name, "group", "term", "statistic")), lapply(est, colnames))
  bycols <- Reduce(intersect, bycols)
  f <- function(x, y) merge(x, y, all = TRUE, sort = FALSE,
                            by = bycols)
  est <- Reduce(f, est)

  # warn that `shape` might be needed
  if (is.null(shape$group_name)) {
    idx <- paste(est$term, est$statistic)
    if (anyDuplicated(idx) > 0) {
      candidate_groups <- sapply(msl, function(x) colnames(x[["tidy"]]))
      candidate_groups <- unlist(candidate_groups)
      candidate_groups <- setdiff(
        candidate_groups,
        c("term", "type", "estimate", "std.error", "conf.level", "conf.low", "conf.high", "statistic", "df.error", "p.value"))
      msg <- c(
        "There are duplicate term names in the table.",
        "The `shape` argument of the `modelsummary` function can be used to print related terms together. The `group_map` argument can be used to reorder, subset, and rename group identifiers. See `?modelsummary` for details.",
        "You can find the group identifier to use in the `shape` argument by calling `get_estimates()` on one of your models. Candidates include:",
        paste(candidate_groups, collapse = ", "))
      insight::format_warning(msg)
    }
  }

  est <- shape_estimates(est, shape, conf_level = conf_level, statistic = statistic, estimate = estimate)

  # distinguish between estimates and gof (first column for tests)
  est$part <- "estimates"
  est <- est[, unique(c("part", names(est)))]

  # empty cells (needed for factor sorting
  est[is.na(est)] <- ""

  # sort rows using factor trick
  if ("term" %in% colnames(est)) {
    if (!is.null(coef_map)) {
        term_order <- coef_map
    }
    est$term <- factor(est$term, unique(term_order))

    if ("group" %in% colnames(est)) {
      if (!is.null(group_map)) {
          est$group <- factor(est$group, group_map)
      } else {
          est$group <- factor(est$group, unique(est$group))
      }
    }

  } else if ("model" %in% colnames(est)) {
    est$model <- factor(est$model, model_names)
  }

  # not statistic column when group=term~model+statistic
  if ("statistic" %in% colnames(est)) {
    est$statistic <- factor(est$statistic, statistic_order)
  }

  est <- est[do.call(order, as.list(est)), ]

  # coef_omit is numeric: by position in the final table, not before merging
  if (is.numeric(coef_omit)) {
    coef_omit <- unique(round(coef_omit))

    if (length(unique(sign(coef_omit))) != 1) {
      insight::format_error("All elements of `coef_omit` must have the same sign.")
    }

    if (!"term" %in% shape$lhs) {
      msg <- "`term` must be on the left-hand side of the `shape` formula when `coef_omit` is a numeric vector."
      insight::format_error(msg)
    }

    term_idx <- paste(est$group, est$term)
    if (max(abs(coef_omit)) > length(unique(term_idx))) {
      msg <- sprintf("There are %s unique terms, but `coef_omit` tried to omit more than that.", length(term_idx))
      insight::format_error(msg)
    }

    idx <- !term_idx %in% unique(term_idx)[abs(coef_omit)]

    if (any(coef_omit > 0)) {
      est <- est[idx, , drop = FALSE]
    } else {
      est <- est[!idx, , drop = FALSE]
    }
  }

  # coef_rename is an unnamed vector: by position in the final table, not before merging
  if (isTRUE(checkmate::check_character(coef_rename, names = "unnamed"))) {
    nterms <- length(unique(est$term))
    if (length(coef_rename) != nterms) {
      msg <- "`coef_rename` must be a named character vector or an unnamed vector of length %s"
      insight::format_error(sprintf(msg, nterms))
    }
    dict <- stats::setNames(coef_rename, as.character(unique(est$term)))
    tmp <- replace_dict(as.character(est$term), dict)
    est$term <- factor(tmp, unique(tmp))
  }

  # we kept the group column until here for sorting of mixed-effects by group
  if (is.null(shape$group_name)) {
    est[["group"]] <- NULL
  }

  # character for binding
  cols <- intersect(
    colnames(est),
    c("term", shape$group_name, "model", "statistic"))
  for (col in cols) {
    est[[col]] <- as.character(est[[col]])
  }

  # in {marginaleffects} objects, `term` is sometimes unique and useless
  if (all(est[["term"]] == "cross")) {
    est[["term"]] <- NULL
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

  gof <- map_gof(gof, gof_omit, gof_map)

  # combine estimates and gof
  tab <- bind_est_gof(est, gof)

  ##################
  #  output table  #
  ##################

  # empty cells
  tab[is.na(tab)] <- ''

  # interaction : becomes ×
  if (is.null(coef_map) &&
      isFALSE(coef_rename) &&
      "term" %in% colnames(tab) &&
      !settings_equal("output_format", "rtf")) {
    idx <- tab$part != 'gof'
    # catch for fixest `i()` operator
    tab$term <- ifelse(idx, gsub('::', ' = ', tab$term), tab$term)
    # conventional interaction
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
  stars_note <- settings_get("stars_note")
  if (isTRUE(stars_note) && !isFALSE(stars) && !any(grepl("\\{stars\\}", c(estimate, statistic)))) {
    stars_note <- make_stars_note(stars)
    if (is.null(notes)) {
      notes <- stars_note
    } else {
      notes <- c(stars_note, notes)
    }
  }

  # data.frame output keeps redundant info
  if (settings_equal("function_called", "modelsummary_rbind")) {
    tab <- redundant_labels(tab, "term")
  }

  if (!settings_equal("output_format", "dataframe") && !settings_equal("function_called", "modelsummary_rbind")) {

    dups <- c("term", "model", shape$group_name)
    for (d in dups) {
      tab <- redundant_labels(tab, d)
    }

    # after label redundancy, before align
    tab$statistic <- tab$part <- NULL


    # HACK: arbitrary spaces to avoid name conflict
    if ("term" %in% colnames(tab)) colnames(tab)[colnames(tab) == "term"]   <- "       "
    if ("model" %in% colnames(tab)) colnames(tab)[colnames(tab) == "model"] <- "         "
    if ("group" %in% colnames(tab)) colnames(tab)[colnames(tab) == "model"] <- "          "
  }

  if (length(unique(tab$group)) == 1) {
    tab$group <- NULL
  }


  # only show group label if it is a row-property (lhs of the group formula)
  tmp <- setdiff(shape$lhs, c("model", "term"))
  if (length(tmp) == 0) {
    tab$group <- NULL
  } else if (!settings_equal("output_format", "dataframe")) {
    colnames(tab)[colnames(tab) == "group"] <- "        "
  }


  # align
  if (is.null(align)) {
    n_stub <- sum(grepl("^ *$", colnames(tab))) +
              sum(colnames(tab) %in% c(" ", shape$group_name))
    align <- paste0(strrep("l", n_stub), strrep("c", ncol(tab) - n_stub))
    if (isTRUE(checkmate::check_data_frame(add_columns))) {
      align <- paste0(align, strrep("c", ncol(add_columns)))
    }
  }


  # {marginaleffects} hack
  flag <- any(sapply(models, inherits, c("marginaleffects", "comparisons", "marginalmeans")))
  if (isTRUE(flag)) {
    colnames(tab) <- gsub("^value$", " ", colnames(tab)) # marginalmeans()
    colnames(tab) <- gsub("^contrast_", "", colnames(tab)) # comparisons() and marginaleffects()
    colnames(tab) <- gsub("^contrast$", " ", colnames(tab)) # comparisons() and marginaleffects()
  }

  # HACK: remove "empty" confidence intervals or standard errors and omit empty rows
  for (i in seq_along(tab)) {
    tab[[i]] <- gsub("\\(\\s*\\)", "", tab[[i]])
    tab[[i]] <- gsub("\\(\\\\num\\{NA\\}\\)", "", tab[[i]])
    tab[[i]] <- gsub("\\[,\\s*\\]", "", tab[[i]])
    tab[[i]] <- gsub("\\[\\\\num\\{NA\\}, \\\\num\\{NA\\}\\]", "", tab[[i]])
    # Issue #560 don't replace fe1^fe2 -> fe1\textasciicircum{}fe2 -> fe1\textasciicircumfe2
    # commented out because I don't see a problem without, and because this line breaks I(wt^2) (Issue #693)
    # tab[[i]] <- gsub("^\\S*\\{\\}\\S*", "", tab[[i]])
  }

  idx <- apply(tab, 1, function(x) any(x != ""))
  tab <- tab[idx, ]

  ## build table
  out <- factory(
    tab,
    align    = align,
    fmt      = fmt,
    hrule    = hrule,
    notes    = notes,
    output   = output,
    title    = title,
    add_rows = add_rows,
    add_columns = add_columns,
    escape = escape,
    ...
  )

  # after factory call
  out <- set_span_cbind(out, span_cbind)

  # invisible return
  if (settings_equal("function_called", "modelsummary_rbind")) {
    return(out)
  } else if (!is.null(settings_get("output_file")) ||
      isTRUE(output == "jupyter") ||
      (isTRUE(output == "default") && settings_equal("output_default", "jupyter"))) {
    settings_rm()
    return(invisible(out))
  # visible return
  } else {
    settings_rm()
    return(out)
  }

}


get_list_of_modelsummary_lists <- function(models, conf_level, vcov, gof_map, gof_function, shape, coef_rename, ...) {

    number_of_models <- max(length(models), length(vcov))

    inner_loop <- function(i) {
        # recycling when 1 model and many vcov
        j <- ifelse(length(models) == 1, 1, i)

        if (inherits(models[[j]], "modelsummary_list")) {
            out <- list(
                tidy = models[[j]][["tidy"]],
                glance = models[[j]][["glance"]])
            return(out)
        }

        # don't waste time if we are going to exclude all gof anyway
        gla <- get_gof(models[[j]], vcov_type = names(vcov)[i], gof_map = gof_map, gof_function = gof_function, ...)

        tid <- get_estimates(
            models[[j]],
            conf_level = conf_level,
            vcov = vcov[[i]],
            shape = shape,
            coef_rename = coef_rename,
            ...)

        out <- list("tidy" = tid, "glance" = gla)
        class(out) <- "modelsummary_list"
        return(out)
    }

    # {parallel}
    dots <- list(...)
    if ("mc.cores" %in% names(dots)) {
        out <- parallel::mclapply(seq_len(number_of_models), inner_loop, mc.cores = dots[["mc.cores"]])

    # {future}
    } else if (isTRUE(check_dependency("future.apply")) &&
               future::nbrOfWorkers() > 1 &&
               number_of_models > 1 &&
               isTRUE(getOption("modelsummary_future", default = TRUE))) {
        # Issue #647: conflict with `furrr`. Very hard to diagnose.
        out <- try(
          future.apply::future_lapply(seq_len(number_of_models), inner_loop, future.seed = TRUE),
          silent = TRUE)
        if (inherits(out, "try-error")) {
          out <- lapply(seq_len(number_of_models), inner_loop)
        }

    # sequential
    } else {
        out <- lapply(seq_len(number_of_models), inner_loop)
    }

   return(out)
}


redundant_labels <- function(dat, column) {
  if (!column %in% colnames(dat)) {
    return(dat)
  }
  # Issue #558: 1-row estimates table with no gof
  if (nrow(dat) > 1) {
    for (i in nrow(dat):2) {
      if (dat$part[i] == "estimates" &&
        dat[[column]][i - 1] == dat[[column]][i]) {
        dat[[column]][i] <- ""
      }
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


