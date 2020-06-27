#' Table 1: Summary statistics for different subsets of the data (e.g., control
#' and treatment groups)
#' 
#' @param formula 1-side formula with a single factor, character, or logical
#'   variable on the right-hand side.
#' @inheritParams modelsummary
#' @inheritParams datasummary
#' @export
#' @examples
#' datasummary_table1(~am, mtcars)
#' @keywords internal
datasummary_table1 <- function(formula,
                               data,
                               output = 'default',
                               fmt = '%.1f',
                               title = NULL,
                               notes = NULL,
                               align = NULL,
                               ...) {

    # output: factory, file, format
    output_list <- parse_output_arg(output)

    # no left-hand support in this template
    sanity_ds_right_handed_formula(formula)
        
    # convert logical and character to factor
    data <- data %>% 
            dplyr::mutate(dplyr::across(where(is.logical) |
                                        where(is.character),
                                        factor))
          
    # RHS variable
    rhs <- labels(stats::terms(formula))

    # RHS must be in data
    if (!rhs %in% colnames(data)) {
        stop('Variable ', rhs, ' must be in data.')
    }

    # RHS must be factor
    if (!is.factor(data[[rhs]])) {
        data[[rhs]] <- factor(data[[rhs]])
    }

    # RHS must have fewer than 10 levels
    if (length(unique(data[[rhs]])) > 10) {
        stop('Each value of the ', rhs, ' variable will create two separate columns. This variable has too many different values to produce a readable table.')
    }

    # data for All() must exclude RHS other wise it appears in rows and columns
    data_no_rhs <- data[, colnames(data) != rhs, drop = FALSE]
    
    # no factor variables
    any_factor <- any(sapply(data_no_rhs, is.factor))
    any_numeric <- any(sapply(data_no_rhs, is.numeric))

    if (any_numeric) {
        fmt_sd <- paste0('(', fmt, ')')
        CleanSD <- function(x) sprintf(fmt_sd, stats::sd(x, na.rm = TRUE))
        CleanMean <- function(x) sprintf(fmt, mean(x, na.rm = TRUE))
        f <- '(` `=1) * Literal("Mean (Std.Dev.)") + All(data_no_rhs) ~ RHS * ((` ` = CleanMean) + (` `=CleanSD)) '
        f <- formula(stringr::str_replace(f, 'RHS', rhs))
        tab_numeric <- tables::tabular(f, data)
    } else {
        tab_numeric <- NULL
    }

    if (any_factor) {
        cleanpct <- function(x, y) sprintf('(%.0f%%)', length(x) / length(y) * 100)
        f <- 'Heading("") * 1 * Literal("N (%)") + All(data_no_rhs, numeric=FALSE, factor=TRUE) ~ RHS * ((` `=1) + (` `=Percent(fn=cleanpct, denom="col")))'
        f <- formula(stringr::str_replace(f, 'RHS', rhs))
        tab_factor <- tabular(f, data)
    } else {
        tab_factor <- NULL
    }

    if (!is.null(tab_numeric) & !is.null(tab_factor)) {
        # make sure both tables have same number of columns
        checkmate::assert_true(ncol(tab_numeric) == 
                                  ncol(tab_factor))
        checkmate::assert_true(ncol(attributes(tab_numeric)$colLabels) ==
                                  ncol(attributes(tab_factor)$colLabels))

        # hack: force both tables to have identical columns and join them
        attributes(tab_numeric)$colLabels <- attributes(tab_factor)$colLabels
    }

    tab <- rbind(tab_numeric, tab_factor)

    # extract content
    dse <- datasummary_extract(tab, sparse_header=TRUE)
    
    factory(dse,
            align = align,
            hrule = NULL,
            notes = notes, 
            output = output,
            title = title)
    
}
