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

    data$dsgroupvar <- data[[rhs]]
    data$dsgroupvar_long <- data[[rhs]]
    data <- data %>% 
            dplyr::group_by(dsgroupvar) %>%
            dplyr::mutate(dsgroupvarn = paste0(dsgroupvar_long, 
                                               ' (N=', dplyr::n(), ')'),
                          dsgroupvarn = factor(dsgroupvarn)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-dsgroupvar)
    
    # no factor variables
    any_factor <- any(sapply(data_no_rhs, is.factor))
    any_numeric <- any(sapply(data_no_rhs, is.numeric))

    if (any_factor) {
        cleanpct <- function(x, y) sprintf('(%.0f)', length(x) / length(y) * 100)
        if (any_numeric) {
            f <- All(data_no_rhs, numeric=FALSE, factor=TRUE) ~ dsgroupvarn * ((Mean=1) + (`(SD)`=Percent(fn=cleanpct, denom="col")))
        } else {
            f <- All(data_no_rhs, numeric=FALSE, factor=TRUE) ~ dsgroupvarn * ((N=1) + (`(%)`=Percent(fn=cleanpct, denom="col")))
        }
        tab_factor <- tabular(f, data)
    } else {
        tab_factor <- NULL
    }
    
    if (any_numeric) {
        fmt_sd <- paste0('(', fmt, ')')
        Mean <- function(x) sprintf(fmt, mean(x, na.rm = TRUE))
        SD <- function(x) sprintf(fmt_sd, stats::sd(x, na.rm = TRUE))
        f <- All(data_no_rhs) ~ dsgroupvarn * (Mean + (`(SD)`=SD))
        tab_numeric <- tables::tabular(f, data)
        if (any_factor && (ncol(as.matrix(tab_numeric)) < ncol(as.matrix(tab_factor)))) {
            f <- All(data_no_rhs) * Heading("") * 1 ~ dsgroupvarn * (Mean + (`(SD)`=SD))
            tab_numeric <- tables::tabular(f, data)
        }
    } else {
        tab_numeric <- NULL
    }
    
    tab <- rbind(tab_numeric, tab_factor)
    tab <- datasummary_extract(tab)
    
    if (any_factor && any_numeric) {
        header <- tab[1, , drop = FALSE]
        for (i in 1:ncol(header)) {
            header[[i]] <- dplyr::case_when(colnames(tab)[i] == 'Mean' ~ 'N',
                                            colnames(tab)[i] == '(SD)' ~ '(%)',
                                            TRUE ~ colnames(tab)[i])
        }
        colnames(tab) <- colnames(header) <- pad(colnames(tab))
        tab <- dplyr::add_row(tab, header, .after = nrow(tab_numeric))
    }
    
    if (is.null(align)) {
        idx <- attr(tab, 'stub_width')
        align <- paste0(strrep('l', idx), 
                        strrep('rl', (ncol(tab) - idx) / 2))
    }
    hrule <- nrow(tab_numeric) + 1

    out <- factory(tab,
                   align = align,
                   hrule = hrule,
                   notes = notes, 
                   output = output,
                   title = title)
    
}
