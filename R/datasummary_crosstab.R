#' Cross tabulations for categorical variables
#'
#' Convenience function to tabulate counts, cell percentages, and row/column
#' percentages for categorical variables. This a wrapper around \link{datasummary}.
#' For more complex cross tabulations, use \link{datasummary} directly.
#'
#' @inheritParams datasummary
#' @import tables
#' @param formula A two-sided formula to describe the table: rows ~ columns, where
#' rows and columns are variables in the data. Both rows and column may contain
#' interactions, e.g., `var1 * var2 ~ var3`.
#' @param statistic A formula of the form `1 ~ 1 + N + Percent("row")`. The left-hand
#' side may only be empty or contain a `1` to include row totals.
#' The right-hand side may contain: `1` for column totals, `N` for counts, `Percent()` for
#' cell percentages, `Percent("row")` for row percentages, `Percent("col")` for column
#' percentages.
#' @examples
#'
#' \dontrun{
#'   # crosstab of two variables, showing counts, row percentages, and row/column totals
#'   datasummary_crosstab(cyl ~ gear, data = mtcars)
#'   # crosstab of two variables, showing counts only and no totals
#'   datasummary_crosstab(cyl ~ gear, statistic = ~ N, data = mtcars)
#'   # crosstab of two interacted variables
#'   datasummary_crosstab(am * cyl ~ gear, data = mtcars)
#' }
#'
#' @details
#' Variables in `formula` are automatically wrapped in `Factor()`.
#' @export
datasummary_crosstab <- function(formula,
                                 statistic = 1 ~ 1 + N + Percent("row"),
                                 data,
                                 output = 'default',
                                 fmt = 1,
                                 title = NULL,
                                 notes = NULL,
                                 align = NULL,
                                 add_columns = NULL,
                                 add_rows = NULL,
                                 sparse_header = FALSE,
                                 ...) {
    # argument checking
    checkmate::assert_formula(formula)
    checkmate::assert_formula(statistic)
    checkmate::assert_data_frame(data, min.rows = 1, min.cols = 1)
    # `formula` may not contain +
    formula_str <- deparse(formula, width.cutoff = 500)
    if (grepl("+", formula_str, fixed = TRUE)) {
        stop("`formula` may not contain variables connected by +, only interactions with * are allowed.")
    }
    # `formula` must be length 3
    if (length(formula) != 3) {
        stop("`formula` needs to be a two-sided formula, e.g. var1 ~ var2.")
    }
    # check statistic formula
    lhs_statistic <- ifelse(length(statistic) == 2, "", deparse(statistic[[2]]))
    if (!(lhs_statistic %in% c("", ".", "1"))) {
        stop("The left-hand side of `statistic` must either be empty of 1.")
    }
    statistic_terms <- stats::terms(statistic)
    allowed <- c("N", "Percent()", 'Percent("row")', 'Percent("col")')
    if (!all(labels(statistic_terms) %in% allowed)) {
        stop("The right-hand side of `statistic` may only contain 1, N, Percent(), Percent('row'), or Percent('col').")
    }
    rhs_statistic <- utils::tail(as.character(statistic), 1)
    if (grepl("*", rhs_statistic, fixed = TRUE)) {
        stop("`statistic` may not contain interactions.")
    }

    # find out if row/column totals should be included
    total_row <- ifelse(lhs_statistic == "1", " + 1", "")
    rhs <- unlist(strsplit(rhs_statistic, "+", fixed = TRUE))
    total_col <- ifelse("1" %in% trimws(rhs), " + 1", "")

    # adjust labels for %
    labels <- labels(statistic_terms)
    labels[labels == 'Percent()'] <- 'Heading("%")*Percent()'
    labels[labels == 'Percent("row")'] <- 'Heading("% row")*Percent("row")'
    labels[labels == 'Percent("col")'] <- 'Heading("% col")*Percent("col")'

    # treat all variables as Factors
    lhs_formula <- paste0("Factor(", all.vars(formula[[2]]), ")")
    rhs_formula <- paste0("Factor(", all.vars(formula[[3]]), ")")

    d_formula <- sprintf("(%s%s) * (%s) ~ %s%s",
        paste(lhs_formula, collapse = " * "), total_row,
        paste(labels, collapse = " + "),
        paste(rhs_formula, collapse = " * "), total_col)

    datasummary(formula = stats::as.formula(d_formula),
                data = data,
                output = output,
                fmt = fmt,
                title = title,
                notes = notes,
                align = align,
                add_columns = add_columns,
                add_rows = add_rows,
                sparse_header = sparse_header,
                ...)
}
