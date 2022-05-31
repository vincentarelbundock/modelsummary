# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'part', 'estimate', 'conf.high', 'conf.low',
'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik',
'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model',
'tmp_grp', 'condition_variable', 'conf_int', 'conf_level'))



#' Model Summary Tables
#'
#' Create beautiful and customizable tables to summarize several statistical
#' models side-by-side. This function supports dozens of statistical models,
#' and it can produce tables in HTML, LaTeX, Word, Markdown, PDF, PowerPoint,
#' Excel, RTF, JPG, or PNG. The appearance of the tables can be customized
#' extensively by specifying the `output` argument, and by using functions from
#' one of the supported table customization packages: `kableExtra`, `gt`,
#' `flextable`, `huxtable`. For more information, see the Details and Examples
#' sections below, and the vignettes on the `modelsummary` website:
#' https://vincentarelbundock.github.io/modelsummary/
#' * [The `modelsummary` Vignette includes dozens of examples of tables with extensive customizations.](https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html)
#' * [The Appearance Vignette shows how to modify the look of tables.](https://vincentarelbundock.github.io/modelsummary/articles/appearance.html)
#'
#' @template modelsummary_details
#'
#' @template options
#'
#' @template modelsummary_parallel
#' 
#' @template modelsummary_examples
#'
#' @param models a model or (optionally named) list of models
#' @param output filename or object type (character string)
#' * Supported filename extensions: .docx, .html, .tex, .md, .txt, .png, .jpg.
#' * Supported object types: "default", "html", "markdown", "latex", "latex_tabular", "data.frame", "gt", "kableExtra", "huxtable", "flextable", "jupyter". The "modelsummary_list" value produces a lightweight object which can be saved and fed back to the `modelsummary` function.
#' * Warning: Users should not supply a file name to the `output` argument if they intend to customize the table with external packages. See the 'Details' section.
#' * LaTeX compilation requires the `booktabs` and `siunitx` packages, but `siunitx` can be disabled or replaced with global options. See the 'Details' section.
#' * The default output formats and table-making packages can be modified with global options. See the 'Details' section.
#' @param fmt determines how to format numeric values
#' * integer: the number of digits to keep after the period `format(round(x, fmt), nsmall=fmt)`
#' * character: passed to the `sprintf` function (e.g., '%.3f' keeps 3 digits with trailing zero). See `?sprintf`
#' * function: returns a formatted character string.
#' * NULL: does not format numbers, which allows users to include function in the "glue" strings in the `estimate` and `statistic` arguments. 
#' * A named list to format distinct elements of the table differently. Names correspond to column names produced by `get_estimates(model)` or `get_gof(model)`. Values are integers, characters, or functions, as described above. The `fmt` element is used as default for unspecified elements Ex: `fmt=list("estimate"=2, "std.error"=1, "r.squared"=4, "fmt"=3)`
#' * LaTeX output: To ensure proper typography, all numeric entries are enclosed in the `\num{}` command, which requires the `siunitx` package to be loaded in the LaTeX preamble. This behavior can be altered with global options. See the 'Details' section.
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
#' * `glue` package strings with braces, with or without R functions, such as:
#'   - `"{p.value} [{conf.low}, {conf.high}]"`
#'   - `"Std.Error: {std.error}"`
#'   - `"{exp(estimate) * std.error}"
#' * Numbers are automatically rounded and converted to strings. To apply functions to their numeric values, as in the last `glue` example, users must set `fmt=NULL`.
#' * Parentheses are added automatically unless the string includes `glue` curly braces `{}`.
#' @param vcov robust standard errors and other manual statistics. The `vcov`
#'   argument accepts six types of input (see the 'Details' and 'Examples'
#'   sections below):
#' * NULL returns the default uncertainty estimates of the model object
#' * string, vector, or (named) list of strings. "iid", "classical", and "constant" are aliases for `NULL`, which returns the model's default uncertainty estimates. The strings "HC", "HC0", "HC1" (alias: "stata"), "HC2", "HC3" (alias: "robust"), "HC4", "HC4m", "HC5", "HAC", "NeweyWest", "Andrews", "panel-corrected", "outer-product", and "weave" use variance-covariance matrices computed using functions from the `sandwich` package, or equivalent method. The behavior of those functions can (and sometimes *must*) be altered by passing arguments to `sandwich` directly from `modelsummary` through the ellipsis (`...`), but it is safer to define your own custom functions as described in the next bullet.
#' * function or (named) list of functions which return variance-covariance matrices with row and column names equal to the names of your coefficient estimates (e.g., `stats::vcov`, `sandwich::vcovHC`, `function(x) vcovPC(x, cluster="country")`).
#' * formula or (named) list of formulas with the cluster variable(s) on the right-hand side (e.g., ~clusterid).
#' * named list of `length(models)` variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#' * a named list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. Warning: since this list of vectors can include arbitrary strings or numbers, `modelsummary` cannot automatically calculate p values. The `stars` argument may thus use incorrect significance thresholds when `vcov` is a list of vectors.
#' @param conf_level confidence level to use for confidence intervals
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
#' @param coef_omit string regular expression (perl-compatible) used to determine which coefficients to omit from the table. A "negative lookahead" can be used to specify which coefficients to *keep* in the table. Examples:
#' * `"ei"`: omit coefficients matching the "ei" substring.
#' * `"^Volume$"`: omit the "Volume" coefficient.
#' * `"ei|rc"`: omit coefficients matching either the "ei" or the "rc" substrings.
#' * `"^(?!Vol)"`: keep coefficients starting with "Vol" (inverse match using a negative lookahead).
#' * `"^(?!.*ei)"`: keep coefficients matching the "ei" substring.
#' * `"^(?!.*ei|.*pt)"`: keep coefficients matching either the "ei" or the "pt" substrings.
#' * See the Examples section below for complete code.
#' @param coef_rename named character vector or function
#' * Named character vector: Values refer to the variable names that will appear in the table. Names refer to the original term names stored in the model object. Ex: c("hp:mpg"="hp X mpg") 
#' * Function: Accepts a character vector of the model's term names and returns a named vector like the one described above. The `modelsummary` package supplies a `coef_rename()` function which can do common cleaning tasks: `modelsummary(model, coef_rename = coef_rename)`
#' @param gof_map rename, reorder, and omit goodness-of-fit statistics and other
#'   model information. This argument accepts 4 types of values:
#' * NULL (default): the `modelsummary::gof_map` dictionary is used for formatting, and all unknown statistic are included.
#' * NA: excludes all statistics from the bottom part of the table.
#' * character vector such as `c("rmse", "nobs", "r.squared")`. Elements correspond to colnames in the data.frame produced by `get_gof(model)`. The default dictionary is used to format and rename statistics.
#' * data.frame with 3 columns named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples' section below.
#' * list of lists, each of which includes 3 elements named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples section below'.
#' @param gof_omit string regular expression (perl-compatible) used to determine which statistics to omit from the bottom section of the table. A "negative lookahead" can be used to specify which statistics to *keep* in the table. Examples:
#' * `"IC"`: omit statistics matching the "IC" substring.
#' * `"BIC|AIC"`: omit statistics matching the "AIC" or "BIC" substrings.
#' * `"^(?!.*IC)"`: keep statistics matching the "IC" substring. 
#' @param group_map named or unnamed character vector. Subset, rename, and
#' reorder coefficient groups specified a grouping variable specified in the
#' `shape` argument formula. This argument behaves like `coef_map`.
#' @param shape formula which determines the shape of the table. The left side
#' determines what appears on rows, and the right side determines what appears
#' on columns. The formula can include a group identifier to display related terms
#' together, which can be useful for models with multivariate outcomes or
#' grouped coefficients (See examples section below). This identifier must be
#' one of the column names produced by: `get_estimates(model)`. If an
#' incomplete formula is supplied (e.g., `~statistic`), `modelsummary` tries to
#' complete it automatically. Potential `shape` values include:
#' * `term + statistic ~ model`: default
#' * `term ~ model + statistic`: statistics in separate columns
#' * `model + statistic ~ term`: models in rows and terms in columns
#' * `term + response + statistic ~ model`: 
#' * `term ~ response`
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
#' * "d": dot-aligned column. Only supported for LaTeX/PDF tables produced by `kableExtra`. These commands must appear in the LaTeX preamble (they are added automatically when compiling Rmarkdown documents to PDF):
#'   - `\usepackage{booktabs}`
#'   - `\usepackage{siunitx}`
#'   - `\newcolumntype{d}{S[input-symbols = ()]}`
#' @param escape boolean TRUE escapes or substitutes LaTeX/HTML characters which could
#' prevent the file from compiling/displaying. This setting does not affect captions or notes.
#' @param ... all other arguments are passed through to the extractor and
#' table-making functions (by default `broom::tidy` and `kableExtra::kbl`, but
#' this can be customized). This allows users to pass arguments directly to
#' `modelsummary` in order to affect the behavior of other functions behind the
#' scenes. For example,
#' * `metrics="none"`, `metrics="all"`, or `metrics=c("R2", "RMSE")` to select the goodness-of-fit extracted by the `performance` package (must have set `options(modelsummary_get="easystats")` first). This can be useful for some models when statistics take a long time to compute. See `?performance::performance`
#' @return a regression table in a format determined by the `output` argument.
#' @importFrom generics glance tidy
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
  coef_rename = NULL,
  gof_map     = NULL,
  gof_omit    = NULL,
  group_map   = NULL,
  add_rows    = NULL,
  align       = NULL,
  notes       = NULL,
  title       = NULL,
  escape      = TRUE,
  ...) {

  ## settings
  settings_init(settings = list(
     "function_called" = "modelsummary"
  ))

  # bug: deprecated `group` argument gets partial matched
  scall <- sys.call()
  if (all(c("group", "shape") %in% names(scall))) {
    stop("The `group` argument is deprecated. Please use `shape` instead.", call. = FALSE)
  # both group and group_map -> group is pushed to ...
  } else if ("group" %in% names(scall) && "group_map" %in% names(scall)) {
    shape <- list(...)[["group"]]
  # only group -> partial match assigns to group_map
  } else if ("group" %in% names(scall) && !"group_map" %in% names(scall)) {
    shape <- group_map
    group_map <- NULL
  }


  ## sanity functions validate variables/settings
  ## sanitize functions validate & modify & initialize
  checkmate::assert_string(gof_omit, null.ok = TRUE)
  sanitize_output(output)           # early
  sanitize_escape(escape)
  sanity_ellipsis(vcov, ...)        # before sanitize_vcov
  models <- sanitize_models(models) # before sanitize_vcov
  vcov <- sanitize_vcov(vcov, length(models), ...)
  number_of_models <- max(length(models), length(vcov))
  estimate <- sanitize_estimate(estimate, number_of_models)
  exponentiate <- sanitize_exponentiate(exponentiate, number_of_models)
  shape <- sanitize_shape(shape)
  statistic <- sanitize_statistic(statistic, shape) # after shape
  gof_map <- sanitize_gof_map(gof_map)
  fmt <- sanitize_fmt(fmt, calling_function = "modelsummary")
  sanity_group_map(group_map)
  sanity_conf_level(conf_level)
  sanity_coef(coef_map, coef_rename, coef_omit)
  sanity_stars(stars)
  sanity_align(align, estimate = estimate, statistic = statistic, stars = stars)

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
  if (isTRUE(escape)) {
     model_names <- escape_string(model_names)
  }


  #######################
  #  modelsummary_list  #
  #######################
  msl <- get_list_of_modelsummary_lists(models = models,
                                        conf_level = conf_level,
                                        vcov = vcov,
                                        gof_map = gof_map, # check if we can skip all gof computation
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

    colnames(tmp)[4] <- model_names[i]

    est[[model_names[i]]] <- tmp

  }

  term_order <- unique(unlist(lapply(est, function(x) x$term)))
  group_order <- unique(unlist(lapply(est, function(x) x$group)))
  statistic_order <- unique(unlist(lapply(est, function(x) x$statistic)))

  f <- function(x, y) merge(x, y, all = TRUE, sort = FALSE,
                            by = c("group", "term", "statistic"))
  est <- Reduce(f, est)

  # warn that `shape` might be needed
  if (is.null(shape$group_name)) {
    idx <- paste(est$term, est$statistic)
    if (anyDuplicated(idx) > 0) {
      candidate_groups <- sapply(msl, function(x) colnames(x[["tidy"]]))
      candidate_groups <- unlist(candidate_groups)
      candidate_groups <- setdiff(
        candidate_groups,
        c("term", "estimate", "std.error", "conf.level", "conf.low", "conf.high",
          "statistic", "df.error", "p.value"))
      if (length(candidate_groups) > 0) {
        candidate_groups <- sprintf("Candidate group identifiers include: %s.",
                                    paste(candidate_groups, collapse = ", "))
      } else{
        candidate_groups <- ""
      }
      msg <- sprintf("There are duplicate term names in the table. The `shape` argument of the `modelsummary` function can be used to print related terms together, and to label them appropriately. You can find the group identifier to use in the `shape` argument by calling `get_estimates()` on one of your models. %s See `?modelsummary` for details.", candidate_groups)

      warning(msg, call. = FALSE)
    }
  }

  est <- shape_estimates(est, shape, conf_level = conf_level)

  # distinguish between estimates and gof (first column for tests)
  est$part <- "estimates"
  est <- est[, unique(c("part", names(est)))]

  # empty cells (needed for factor sorting
  est[is.na(est)] <- ""

  # sort rows using factor trick
  if ("term" %in% colnames(est)) {
    if (!is.null(coef_map)) {
        term_order <- coef_map
        if (isTRUE(escape)) {
            term_order <- escape_string(term_order)
        }
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

  # character for binding
  for (col in c("term", "group", "model", "statistic")) {
    if (col %in% colnames(est)) {
      est[[col]] <- as.character(est[[col]])
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
      is.null(coef_rename) &&
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
  if (!settings_equal("output_format", "dataframe")) {

    tab <- redundant_labels(tab, "model")
    tab <- redundant_labels(tab, "group")
    tab <- redundant_labels(tab, "term")

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
    n_stub <- sum(grepl("^ *$", colnames(tab)))
    align <- paste0(strrep("l", n_stub), strrep("c", ncol(tab) - n_stub))
  }

  # HACK: remove "empty" confidence intervals or standard errors and omit empty rows
  for (i in seq_along(tab)) {
    tab[[i]] <- gsub("\\(\\s*\\)", "", tab[[i]])
    tab[[i]] <- gsub("\\(\\\\num\\{NA\\}\\)", "", tab[[i]])
    tab[[i]] <- gsub("\\[,\\s*\\]", "", tab[[i]])
    tab[[i]] <- gsub("\\[\\\\num\\{NA\\}, \\\\num\\{NA\\}\\]", "", tab[[i]])
    tab[[i]] <- gsub("\\{\\}", "", tab[[i]])
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
    ...
  )

  # invisible return
  if (!is.null(settings_get("output_file")) ||
      output == "jupyter" ||
      (output == "default" && settings_equal("output_default", "jupyter"))) {
    settings_rm()
    return(invisible(out))
  # visible return
  } else {
    settings_rm()
    return(out)
  }

}


get_list_of_modelsummary_lists <- function(models, conf_level, vcov, gof_map, ...) {

    number_of_models <- max(length(models), length(vcov))

    vcov_type <- get_vcov_type(vcov)

    vcov_names <- names(vcov)


    # warning for models with hard-coded non-IID vcov
    hardcoded <- c("lm_robust", "iv_robust", "felm")
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
        warning(sprintf('When the `vcov` argument is set to "iid", "classical", or "constant", `modelsummary` extracts the default variance-covariance matrix from the model object. For objects of class `%s`, the default vcov is not always IID. Please make sure that the standard error label matches the numeric results in the table. Note that the `vcov` argument accepts a named list for users who want to customize the standard error labels in their regression tables.', class(models[[j]])[1]),
                call. = FALSE)
    }

    inner_loop <- function(i) {
        # recycling when 1 model and many vcov
        j <- ifelse(length(models) == 1, 1, i)

        # don't waste time if we are going to exclude all gof anyway
        gla <- get_gof(models[[j]], vcov_type[[i]], gof_map = gof_map, ...)

        tid <- get_estimates(models[[j]], conf_level = conf_level, vcov = vcov[[i]], ...)

        out <- list("tidy" = tid, "glance" = gla)
        class(out) <- "modelsummary_list"
        return(out)
    }

    # parallel
    if (isTRUE(check_dependency("future.apply"))) {
        parallel_flag <- !"sequential" %in% attr(future::plan(), "class") && number_of_models > 1
    } else {
        parallel_flag <- FALSE
    }

    if (isTRUE(parallel_flag)) {
        out <- future.apply::future_lapply(seq_len(number_of_models), inner_loop)
    } else {
        out <- lapply(seq_len(number_of_models), inner_loop)
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
