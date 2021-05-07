#' Cross tabulations for categorical variables
#'
#' Convenience function to tabulate counts, cell percentages, and row/column
#' percentages for categorical variables.
#'
#' @param formula A two-sided formula to describe the table: rows ~ columns, where
#' rows and columns are variables in the data. Both rows and column may contain
#' interactions, e.g., var1 * var2 ~ var3.
#' @param statistic A formula of the form 1 ~ 1 + N + Percent("row"). The left-hand
#' side may only be empty or contain a 1 to include row totals.
#' The right-hand side may contain: 1 for column totals, N for counts, Percent() for
#' cell percentages, Percent("row") for row percentages, Percent("col") for column
#' percentages.
#' @param data A data.frame (or tibble)
#' @param sparse_header TRUE or FALSE. TRUE eliminates column headers which
#' have a unique label across all columns, except for the row immediately above
#' the data. FALSE keeps all headers. The order in which terms are entered in
#' the formula determines the order in which headers appear.
#' @param ... passed to datasummary
#' @return a crosstab
#' @details
#' X
#' @examples
#' \dontrun{
#' }
#'
#' @export
datasummary_crosstab <- function(
        formula,
        statistic = 1 ~ 1 + N + Percent("row"),
        data,
        sparse_header = FALSE, ...) {
    checkmate::assert_formula(formula)
    checkmate::assert_formula(statistic)
    checkmate::assert_data_frame(data, min.rows = 1, min.cols = 1)

    lhs <- paste0("Factor(", all.vars(formula[[2]]), ")")
    rhs <- paste0("Factor(", all.vars(formula[[3]]), ")")

    # find out if row/column totals should be included
    terms <- stats::terms(statistic)
    total_row <- ifelse(attr(terms, "response") == 1, " + 1", "")
    indep <- tail(as.character(statistic), 1)
    indep_vars <- unlist(strsplit(indep, "+", fixed = TRUE))
    total_col <- ifelse("1" %in% trimws(indep_vars), " + 1", "")

    # adjust labels for %
    labels <- labels(terms)
    labels[labels == 'Percent()'] <- 'Heading("%")*Percent()'
    labels[labels == 'Percent("row")'] <- 'Heading("% row")*Percent("row")'
    labels[labels == 'Percent("col")'] <- 'Heading("% col")*Percent("col")'

    d_formula <- sprintf("(%s%s) * (%s) ~ %s%s",
        paste(lhs, collapse = " * "), total_row,
        paste(labels, collapse = " + "),
        paste(rhs, collapse = " * "), total_col)
    datasummary(as.formula(d_formula), data, sparse_header = sparse_header, ...)
}
