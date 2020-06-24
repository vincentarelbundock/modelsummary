#' A thin wrapper around `tables::tabular`
#'
#' @inheritParams modelsummary
#' @import tables
#' @param formula A two-sided formula which describes a 'tabular' object.
#'   Details can be found in the 'tables' package documentation:
#'   ?tables::tabular
#' @param data A data.frame (or tibble)
#' @param sparse_header TRUE or FALSE
#' \itemize{
#' \item TRUE eliminates column headers which have a unique label across all
#' columns, except for the row immediately above the data.
#' \item FALSE keeps all headers.
#' \item Note that the order in which terms are entered in the formula
#' determines the order in which headers appear. For example, `x~mean*z` will
#' print the `mean`-related header above the `z`-related header.`
#' }
#' @export
datasummary <- function(formula,
                        data,
                        output = 'default',
                        title = NULL,
                        notes = NULL,
                        sparse_header = TRUE,
                        ...) {
    
    # output: factory, file, format
    output_list <- parse_output_arg(output)

    # convenience: transform logical and character to factor
    # are there use-cases for character variables?
    data <- data %>%
            dplyr::mutate(dplyr::across(where(is.character) |
                                        where(is.logical),
                                        factor))
    
    # factor check
    sanity_ds_nesting_factor(formula, data)
    
    # create table
    tab <- tables::tabular(formula, data)

    # extract content
    dse <- datasummary_extract(tab, sparse_header = sparse_header)
    
    # greenfield
    if (output_list$output_factory == 'gt') {
        main <- dse$gt$main
        span <- dse$gt$span
    } else if (output_list$output_factory == 'kableExtra') {
        if (output_list$output_format == 'markdown') {
            main <- dse$kableExtra_markdown$main
            span <- dse$kableExtra_markdown$span
        } else {
            main <- dse$kableExtra$main
            span <- dse$kableExtra$span
        }
    } else if (output_list$output_factory == 'flextable') {
        main <- dse$flextable$main
        span <- dse$flextable$span
    } else if (output_list$output_factory == 'huxtable') {
        main <- dse$huxtable$main
        span <- dse$huxtable$span
    }
    
    out <- factory(main, 
                   hrule = NULL,
                   notes = notes, 
                   output = output,
                   span = span,
                   title = title)
    
    return(out)

}
