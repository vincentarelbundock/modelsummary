#' Balance table: Summary statistics for different subsets of the data (e.g.,
#' control and treatment groups)
#'
#' @param formula a one-sided formula with the "condition" or "column" variable
#'   on the right-hand side.
#' @param data A data.frame (or tibble). If this data includes columns called
#'   "blocks", "clusters", and/or "weights", the 'estimatr' package will
#'   consider them when calculating the difference in means.
#' @param dinm TRUE calculates a difference in means with uncertainty
#'   estimates. This option is only available if the `estimatr` package is
#'   installed. If `data` includes columns named "blocks", "clusters", or
#'   "weights", this information will be taken into account automatically by
#'   `estimatr::difference_in_means`.
#' @param dinm_statistic string: "std.error" or "p.value"
#' @inheritParams modelsummary
#' @inheritParams datasummary
#' @export
#' @examples
#' \dontrun{
#' datasummary_balance(~am, mtcars)
#' }
#' @keywords internal
datasummary_balance <- function(formula,
                                data,
                                output = 'default',
                                fmt = "%.1f",
                                title = NULL,
                                notes = NULL,
                                align = NULL,
                                add_columns = NULL,
                                dinm = TRUE,
                                dinm_statistic = 'std.error',
                                ...) {
    
    # tables does not play well with tibbles
    data <- as.data.frame(data)

    # functions with formatting: str_replace(fmt) doesn't get picked-up by tabular
    MeanF <- function(x) sprintf(fmt, mean(x, na.rm = TRUE))
    SDF <- function(x) sprintf(fmt, stats::sd(x, na.rm = TRUE))
    DinMestimate <- function(x, data) {
        out <- DinM(x, data = data, statistic = 'estimate')
        sprintf(fmt, out)
    }
    DinMpval <- function(x, data) {
        out <- DinM(x, data = data, statistic = 'p.value')
        if (out < .001) {
            '<.001'
        } else {
            sprintf("%.3f", out)
        }
    }
    DinMstderror <- function(x, data) {
        out <- DinM(x, data = data, statistic = 'std.error')
        sprintf(fmt, out)
    }
    
    # valid input
    checkmate::assert_data_frame(data, min.rows = 1, min.cols = 1)
    checkmate::assert(
        checkmate::check_string(dinm_statistic),
        checkmate::check_true(dinm_statistic %in% c('std.error', 'p.value')),
        combine = 'and'
    )
    
    # convert logical and character to factor
    data <- data %>% 
            dplyr::mutate(dplyr::across(where(is.logical) |
                                        where(is.character),
                                        factor))
    
    
    # no left-hand support in this template
    sanity_ds_right_handed_formula(formula)
    
    # output: factory, file, format
    output_list <- parse_output_arg(output)
    
    # RHS variable
    rhs <- labels(stats::terms(formula))
    
    # RHS must be in data
    if (!rhs %in% colnames(data)) {
        stop('Variable ', rhs, ' must be in data.')
    }
    
    # condition_variable must be factor
    data$condition_variable <- factor(data[[rhs]])
    
    # drop missing conditions
    idx <- !is.na(data$condition_variable)
    data <- data[idx, , drop = FALSE]
    
    # RHS must have fewer than 10 levels
    if (length(unique(data$condition_variable)) > 10) {
        stop('Each value of the ', rhs, ' variable will create two separate columns. This variable has too many different values to produce a readable table.')
    }
    
    # data for All() must exclude these otherwise they appear as rows
    idx <- setdiff(colnames(data), c(rhs, 'condition_variable', 'clusters', 'blocks', 'weights'))
    data_no_condition <- data[, idx, drop = FALSE]
    
    # conditions with (N = ???)
    data <- data %>% 
            dplyr::mutate(tmp_grp = condition_variable) %>%
            dplyr::group_by(tmp_grp) %>%
            dplyr::mutate(condition_variable = paste0(condition_variable, ' (N=', dplyr::n(), ')')) %>%
            dplyr::ungroup() %>%
            dplyr::select(-tmp_grp)
    
    # variable types
    any_factor <- any(sapply(data_no_condition, is.factor))
    any_numeric <- any(sapply(data_no_condition, is.numeric))
    
    # stop if factor has too many levels
    if (any_factor) {
        tmp <- data_no_condition %>% dplyr::select(where(is.factor))
        tmp <- sapply(tmp, function(x) length(unique(x)) > 30)
        if (any(tmp)) {
            stop('Some categorical variables have over 30 levels.' )
        }
    }
    
    # is dinm possible?
    checkmate::assert_flag(dinm)
    if (isTRUE(dinm)) {
        # dinm is only possible when `estimatr` is installed 
        if (!requireNamespace('estimatr', quietly = TRUE)) {
            dinm <- FALSE
            warning('Please install the `estimatr` package, or set `dinm=FALSE` to suppress this warning.')
        } 
        
        # dinm is only possible when the number of conditions < 2
        if (length(unique(data$condition_variable)) != 2) {
            dinm <- FALSE
            warning('Make sure the number of unique conditions equals 2, or set `dinm=FALSE` to suppress this warning.')
        }
        
        # dinm is only possible for numeric variables
        if (!any_numeric) {
            dinm <- FALSE
        }
    }
    
    # function to insert an empty column
    padding <- function(x) " "

    # factors
    tab_fac <- NULL
    
    if (any_numeric) {
        f_fac <- 'All(data_no_condition, numeric=FALSE, factor=TRUE) ~
                  Factor(condition_variable) * (Heading("Mean") * 1 * Format(digits=1) + 
                  Heading("Std. Dev.") * Percent(denom="col") * Format(digits=1))'
    } else {
        f_fac <- 'All(data_no_condition, numeric=FALSE, factor=TRUE) ~
                  Factor(condition_variable) * (Heading("N") * 1 * Format(digits=1) + 
                  Heading("%") * Percent(denom="col") * Format(digits=1))'
    }

    
    if (isTRUE(dinm)) {
        if (dinm_statistic == 'std.error') {
            tmp <- '+ Heading("Diff. in Means") * padding + Heading("Std. Error") * padding'
        } else {
            tmp <- '+ Heading("Diff. in Means") * padding + Heading("p") * padding'
        }
        f_fac <- paste(f_fac, tmp)
    }
    
    if (any_factor) {
        tab_fac <- tables::tabular(f_fac, data)
    }
    
    # numerics
    tab_num <- NULL
    
    f_num <- '~ Factor(condition_variable) * (Heading("Mean") * MeanF +  Heading("Std. Dev.") *  SDF)'

    # empty column if there are factor labels
    stub_fac <- attr(tab_fac, 'rowLabels')
    
    if (!is.null(stub_fac) && (ncol(stub_fac) == 2)) {
        f_num <- paste('All(data_no_condition) * Heading(" ") * 1', f_num)
    } else {
        f_num <- paste('All(data_no_condition)', f_num)
    }
    
    if (isTRUE(dinm)) {
        if (dinm_statistic == 'std.error') {
            f_num <- paste(f_num, '+ Heading("Diff. in Means") * DinMestimate * Arguments(data = data) + 
                                     Heading("Std. Error") * DinMstderror * Arguments(data = data)')
        } else {
            f_num <- paste(f_num, '+ Heading("Diff. in Means") * DinMestimate * Arguments(data = data) + 
                                     Heading("p") * DinMpval * Arguments(data = data)')
        }
    }

    if (any_numeric) {
        tab_num <- tables::tabular(formula(f_num), data)
    }
    
    tab <- rbind(tab_num, tab_fac)
    tab <- datasummary_extract(tab)
    
    # num/fac separator
    if (any_numeric && any_factor) {
        factor_header <- tab[1, , drop = FALSE]
        for (i in seq_along(factor_header)) {
            if (trimws(colnames(tab)[i]) == 'Mean') {
                factor_header[[i]] <- 'N'
            } else if (trimws(colnames(tab)[i]) == 'Std. Dev.') {
                factor_header[[i]] <- '%'
            } else {
                factor_header[[i]] <- ''
            }
        }
        attr(factor_header, 'position') <- nrow(tab_num) + 1
        
        hrule <- nrow(tab_num) + 1
    } else {
        factor_header <- NULL
        hrule <- NULL
    }
    if (inherits(add_columns, 'data.frame')) {
        tmp <- add_columns[1, 1, drop = FALSE]
        for (i in seq_along(add_columns)) {
            tmp[[colnames(add_columns)[i]]] <- ''
        }   
        factor_header <- dplyr::bind_cols(factor_header, tmp)
    }
    
    # column align
    if (is.null(align)) {
        idx <- attr(tab, 'stub_width')
        if (inherits(add_columns, 'data.frame')) {
            tab_width <- ncol(tab) + ncol(add_columns) - idx
        } else {
            tab_width <- ncol(tab) - idx
        }
        align <- paste0(strrep('l', idx), strrep('r', tab_width))
    }
    
    factory(tab,
            align = align,
            hrule = hrule,
            notes = notes, 
            fmt = fmt,
            output = output,
            add_rows = factor_header,
            add_columns = add_columns,
            title = title,
            ...)
    
}




