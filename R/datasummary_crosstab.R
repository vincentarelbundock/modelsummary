#' Cross tabulations for categorical variables
#'
#' Convenience function to tabulate counts, cell percentages, and row/column
#' percentages for categorical variables. See the Details section for a
#' description of the internal design. For more complex cross tabulations, use
#' \link{datasummary} directly. See the Details and Examples sections below,
#' and the vignettes on the `modelsummary` website:
#' * https://modelsummary.com/
#' * https://modelsummary.com/vignettes/datasummary.html
#'
#' @inheritParams datasummary
#' @import tables
#' @param formula A two-sided formula to describe the table: rows ~ columns,
#'   where rows and columns are variables in the data. Rows and columns may
#'   contain interactions, e.g., `var1 * var2 ~ var3`.
#' @param statistic A formula of the form `1 ~ 1 + N + Percent("row")`. The
#'   left-hand side may only be empty or contain a `1` to include row totals.
#'   The right-hand side may contain: `1` for column totals, `N` for counts,
#'   `Percent()` for cell percentages, `Percent("row")` for row percentages,
#'   `Percent("col")` for column percentages.
#' @details `datasummary_crosstab` is a wrapper around the \link{datasummary}
#'   function. This wrapper works by creating a customized formula and by
#'   feeding it to `datasummary`. The customized formula comes in two parts.
#'
#'   First, we take a two-sided formula supplied by the `formula` argument.
#'   All variables of that formula are wrapped in a `Factor()` call to ensure
#'   that the variables are treated as categorical.
#'
#'   Second, the `statistic` argument gives a two-sided formula which specifies
#'   the statistics to include in the table. `datasummary_crosstab` modifies
#'   this formula automatically to include "clean" labels.
#'
#'   Finally, the `formula` and `statistic` formulas are combined into a single
#'   formula which is fed directly to the `datasummary` function to produce the
#'   table.
#' @template kableExtra2tinytable
#' @template citation
#' @template options
#' @section Examples:
#' ```{r, eval = FALSE}
#' library(modelsummary)
#'
#' # crosstab of two variables, showing counts, row percentages, and row/column totals
#' datasummary_crosstab(cyl ~ gear, data = mtcars)
#'
#' # crosstab of two variables, showing counts only and no totals
#' datasummary_crosstab(cyl ~ gear, statistic = ~ N, data = mtcars)
#'
#' # crosstab of three variables
#'   datasummary_crosstab(am * cyl ~ gear, data = mtcars)
#'
#' # crosstab with two variables and column percentages
#' datasummary_crosstab(am ~ gear, statistic = ~ Percent("col"), data = mtcars)
#' ```
#'
#' @details
#' Variables in `formula` are automatically wrapped in `Factor()`.
#' @export
datasummary_crosstab <- function(
  formula,
  statistic = 1 ~ 1 + N + Percent("row"),
  data,
  output = getOption("modelsummary_output", default = "default"),
  fmt = 1,
  title = getOption("modelsummary_title", default = NULL),
  notes = getOption("modelsummary_notes", default = NULL),
  align = getOption("modelsummary_align", default = NULL),
  add_columns = getOption("modelsummary_add_columns", default = NULL),
  add_rows = getOption("modelsummary_add_rows", default = NULL),
  sparse_header = getOption("modelsummary_sparse_header", default = TRUE),
  escape = getOption("modelsummary_escape", default = TRUE),
  ...
) {
  ## settings
  settings_init(
    settings = list(
      "function_called" = "datasummary_crosstab"
    )
  )

  # argument checking
  tmp <- sanitize_output(output) # before sanitize_escape
  output_format <- tmp$output_format
  output_factory <- tmp$output_factory
  output_file <- tmp$output_file
  sanitize_escape(escape) # after sanitize_output

  checkmate::assert_formula(formula)
  checkmate::assert_formula(statistic, null.ok = TRUE)
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 1)

  # `formula` may not contain +
  formula_str <- deparse(formula, width.cutoff = 500)

  if (grepl("+", formula_str, fixed = TRUE)) {
    stop(
      "The `formula` argument of the `datasummary_crosstab` function may not contain variables connected by +, only interactions with * are allowed. To produce more complex tables, consider using the datasummary() function.",
      call. = FALSE
    )
  }

  if (isTRUE(grepl("=|Heading", formula_str))) {
    msg <-
      "The `formula` argument of the `datasummary_crosstab` function does not support the `=` sign or the `Heading()` function. You can rename variables in the data frame before calling `datasummary_crosstab` and use backticks to enclose variable names with spaces. For example:

dat <- mtcars
dat$`# of Cylinders` <- dat$cyl
datasummary_crosstab(`# of Cylinders` ~ am * gear, data = dat)

Note that the `datasummary()` function supports the `=` sign and the `Heading()` function to rename variables. If you would like to contribute code to support those in `datasummary_crosstab`, please visit the `modelsummary` development website: https://github.com/vincentarelbundock/modelsummary
"
    stop(msg, call. = FALSE)
  }

  # `formula` must be length 3
  if (length(formula) != 3) {
    stop(
      "`formula` needs to be a two-sided formula, e.g. var1 ~ var2. To produce more complex tables, consider using the datasummary() function.",
      call. = FALSE
    )
  }

  # check statistic formula
  if (!is.null(statistic)) {
    lhs_statistic <- ifelse(length(statistic) == 2, "", deparse(statistic[[2]]))
    if (!(lhs_statistic %in% c("", ".", "1"))) {
      stop(
        "The left-hand side of `statistic` must either be empty of 1. To produce more complex tables, consider using the datasummary() function."
      )
    }
    rhs_statistic <- utils::tail(as.character(statistic), 1)
    if (grepl("*", rhs_statistic, fixed = TRUE)) {
      stop(
        "`statistic` may not contain interactions. To produce more complex tables, consider using the datasummary() function."
      )
    }
    statistic_terms <- stats::terms(statistic)
    allowed <- c("N", "Percent()", 'Percent("row")', 'Percent("col")')
    if (!all(labels(statistic_terms) %in% allowed)) {
      stop(
        "The right-hand side of `statistic` may only contain 1, N, Percent(), Percent('row'), or Percent('col').To produce more complex tables, consider using the datasummary() function."
      )
    }

    # find out if row/column totals should be included
    total_row <- ifelse(lhs_statistic == "1", " + 1", "")
    rhs <- unlist(strsplit(rhs_statistic, "+", fixed = TRUE))
    total_col <- ifelse("1" %in% trimws(rhs), " + 1", "")

    # adjust labels for %
    labels <- labels(statistic_terms)
    labels[labels == "Percent()"] <- 'Heading("%")*Percent()'
    labels[labels == 'Percent("row")'] <- 'Heading("% row")*Percent("row")'
    labels[labels == 'Percent("col")'] <- 'Heading("% col")*Percent("col")'
  }

  # treat all variables as Factors
  ## lhs_formula <- paste0("Factor(", all.vars(formula[[2]]), ")")
  ## rhs_formula <- paste0("Factor(", all.vars(formula[[3]]), ")")
  lhs_formula <- paste0(all.vars(formula[[2]]))
  rhs_formula <- paste0(all.vars(formula[[3]]))

  # wrap variable names in backticks if they include spaces
  idx <- grepl("\\s", lhs_formula)
  lhs_formula[idx] <- sprintf("`%s`", lhs_formula[idx])
  idx <- grepl("\\s", rhs_formula)
  rhs_formula[idx] <- sprintf("`%s`", rhs_formula[idx])

  lhs_formula <- ifelse(
    sapply(lhs_formula, function(x) !is.factor(data[[x]])),
    sprintf("Factor(%s)", lhs_formula),
    lhs_formula
  )

  rhs_formula <- ifelse(
    sapply(rhs_formula, function(x) !is.factor(data[[x]])),
    sprintf("Factor(%s)", rhs_formula),
    rhs_formula
  )

  if (is.null(statistic)) {
    d_formula <- sprintf(
      "%s ~ %s",
      paste(lhs_formula, collapse = " * "),
      paste(rhs_formula, collapse = " * ")
    )
  } else {
    d_formula <- sprintf(
      "(%s%s) * (%s) ~ %s%s",
      paste(lhs_formula, collapse = " * "),
      total_row,
      paste(labels, collapse = " + "),
      paste(rhs_formula, collapse = " * "),
      total_col
    )
  }

  out <- datasummary(
    formula = stats::as.formula(d_formula),
    data = data,
    output = output,
    fmt = fmt,
    title = title,
    notes = notes,
    align = align,
    add_columns = add_columns,
    add_rows = add_rows,
    sparse_header = sparse_header,
    escape = escape,
    ...
  )

  if (!is.null(output_file)) {
    settings_rm()
    return(invisible(out))
  } else {
    settings_rm()
    return(out)
  }
}
