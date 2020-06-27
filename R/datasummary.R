#' Create summary tables using 2-sided formulae: crosstabs, frequencies, table
#' 1s and more.
#'
#' @inheritParams modelsummary
#' @import tables
#' @param formula A two-sided formula to describe the table: rows ~ columns.
#' See the Examples section for a mini-tutorial and the Details section for
#' more resources.
#' @param data A data.frame (or tibble)
#' @param align A character string of length equal to the number of columns in
#' the table.  "lcr" means that the first column will be left-aligned, the 2nd
#' column center-aligned, and the 3rd column right-aligned.
#' @param sparse_header TRUE or FALSE. TRUE eliminates column headers which
#' have a unique label across all columns, except for the row immediately above
#' the data. FALSE keeps all headers. The order in which terms are entered in
#' the formula determines the order in which headers appear. For example,
#' `x~mean*z` will print the `mean`-related header above the `z`-related
#' header.`
#' @examples
#' 
#' \dontrun{
#' # The left-hand side of the formula describes rows, and the right-hand side
#' # describes columns. This table uses the "mpg" variable as a row and the "mean"
#' # function as a column:
#' 
#' datasummary(mpg ~ mean, data = mtcars)
#' 
#' # This table uses the "mean" function as a row and the "mpg" variable as a column:
#' 
#' datasummary(mean ~ mpg, data = mtcars)
#' 
#' # Display several variables or functions of the data using the "+"
#' # concatenation operator. This table has 2 rows and 2 columns:
#' 
#' datasummary(hp + mpg ~ mean + sd, data = mtcars)
#' 
#' # Nest variables or statistics inside a "factor" variable using the "*" nesting
#' # operator. This table shows the mean of "hp" and "mpg" for each value of
#' # "cyl":
#' 
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' datasummary(hp + mpg ~ cyl * mean, data = mtcars)
#' 
#' # If you don't want to convert your original data
#' to factors, you can use the 'Factor()'
#' # function inside 'datasummary' to obtain an identical result:
#' 
#' datasummary(hp + mpg ~ Factor(cyl) * mean, data = mtcars)
#' 
#' # You can nest several variables or statistics inside a factor by using
#' # parentheses. This table shows the mean and the standard deviation for each
#' # subset of "cyl":
#' 
#' datasummary(hp + mpg ~ cyl * (mean + sd), data = mtcars)
#' 
#' # Summarize all numeric variables with 'All()'
#' datasummary(All(mtcars) ~ mean + sd, data = mtcars)
#' 
#' # Define custom summary statistics. Your custom function should accept a vector
#' # of numeric values and return a single numeric or string value:
#' 
#' minmax <- function(x) sprintf("[%.2f, %.2f]", min(x), max(x))
#' mean_na <- function(x) mean(x, na.rm = TRUE)
#' 
#' datasummary(hp + mpg ~ minmax + mean_na, data = mtcars)
#'
#' # To handle missing values, you can pass arguments to your functions using
#' # '*Arguments()'
#'
#' datasummary(hp + mpg ~ mean * Arguments(na.rm = TRUE), data = mtcars)
#' 
#' # For convenience, 'modelsummary' supplies several convenience functions
#' # with the argument `na.rm=TRUE` by default: Mean, Median, Min, Max, SD, Var,
#' # P0, P25, P50, P75, P100, NUnique, Histogram
#' 
#' datasummary(hp + mpg ~ Mean + SD + Histogram, data = mtcars)
#' 
#' # These functions also accept a 'fmt' argument which allows you to
#' # round/format the results
#' 
#' datasummary(hp + mpg ~ Mean * Arguments(fmt = "%.3f") + SD * Arguments(fmt = "%.1f"), data = mtcars)
#' 
#' # Save your tables to a variety of output formats:
#' f <- hp + mpg ~ Mean + SD
#' datasummary(f, data = mtcars, output = 'table.html')
#' datasummary(f, data = mtcars, output = 'table.tex')
#' datasummary(f, data = mtcars, output = 'table.md')
#' datasummary(f, data = mtcars, output = 'table.docx')
#' datasummary(f, data = mtcars, output = 'table.pptx')
#' datasummary(f, data = mtcars, output = 'table.jpg')
#' datasummary(f, data = mtcars, output = 'table.png')
#' 
#' # Display human-readable code
#' datasummary(f, data = mtcars, output = 'html')
#' datasummary(f, data = mtcars, output = 'markdown')
#' datasummary(f, data = mtcars, output = 'latex')
#' 
#' # Return a table object to customize using a table-making package
#' datasummary(f, data = mtcars, output = 'gt')
#' datasummary(f, data = mtcars, output = 'kableExtra')
#' datasummary(f, data = mtcars, output = 'flextable')
#' datasummary(f, data = mtcars, output = 'huxtable')
#' }
#'
#' @details
#' Visit the 'modelsummary' website for more usage examples:
#' https://vincentarelbundock.github.io/modelsummary
#'
#' The 'datasummary' function is a thin wrapper around the 'tabular' function
#' from the 'tables' package. More details about table-making formulas can be
#' found in the 'tables' package documentation: ?tables::tabular
#' 
#' Hierarchical or "nested" column labels are only available for these output
#' formats: kableExtra, gt, html, rtf, and LaTeX. When saving tables to other
#' formats, nested labels will be combined to a "flat" header.
#' @export
datasummary <- function(formula,
                        data,
                        output = 'default',
                        title = NULL,
                        notes = NULL,
                        align = NULL,
                        sparse_header = TRUE) {
    
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
    
    # align stub l rest r
    if (is.null(align)) {
        align <- paste0(strrep('l', attr(dse, 'stub_width')),
                        strrep('r', ncol(dse) - attr(dse, 'stub_width')))
    }
    
    # build
    out <- factory(dse, 
                   align = align,
                   hrule = NULL,
                   notes = notes, 
                   output = output,
                   title = title)
    
    return(out)

}
