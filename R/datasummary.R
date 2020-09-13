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
#' @param add_columns a data.frame (or tibble) with the same number of rows as
#' your main table.
#' @param sparse_header TRUE or FALSE. TRUE eliminates column headers which
#' have a unique label across all columns, except for the row immediately above
#' the data. FALSE keeps all headers. The order in which terms are entered in
#' the formula determines the order in which headers appear. For example,
#' `x~mean*z` will print the `mean`-related header above the `z`-related
#' header.`
#' @examples
#'
#' \dontrun{
#'
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
#' # to factors, you can use the 'Factor()'
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
#'
#' # add_rows
#' new_rows <- data.frame(a = 1:2, b = 2:3, c = 4:5)
#' attr(new_rows, 'position') <- c(1, 3)
#' datasummary(mpg + hp ~ mean + sd, data = mtcars, add_rows = new_rows)
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
                        fmt = "%.2f",
                        title = NULL,
                        notes = NULL,
                        align = NULL,
                        add_columns = NULL,
                        add_rows = NULL,
                        sparse_header = TRUE) {

  sanity_output(output)

  # output: factory, file, format
  output_list <- parse_output_arg(output)

  # convenience: transform logical and character to factor
  # are there use-cases for character variables?
  data <- data %>%
    dplyr::mutate(dplyr::across(where(is.character) | where(is.logical),
                                factor))

  # tibble -> data.frame (for All())
  data <- as.data.frame(data)

  # factor check
  sanity_ds_nesting_factor(formula, data)

  # create table
  tab <- tryCatch(tables::tabular(formula, data), error = function(e) e)

  # informative error message
  if (inherits(tab, 'error')) {
    if (grepl('Duplicate values:', tab$message)) {
      message('This error often occurs when the "*" nesting operator is used, but none of the nested terms are categorical variables (factor, logical or character types). You can transform your variable in the original data, or wrap it in a Factor() function in the `datasummary` formula.')
    }
    stop(tab$message)
  }

  # extract content
  dse <- datasummary_extract(tab,
    fmt = fmt,
    sparse_header = sparse_header)

  # align stub l rest r
  stub_width <- attr(dse, 'stub_width')
  tab_width <- ncol(dse)
  if (inherits(add_columns, 'data.frame')) {
    tab_width <- tab_width + ncol(add_columns)
  }
  if (is.null(align)) {
    align <- paste0(strrep('l', stub_width),
      strrep('r', tab_width - stub_width))
  }

  # convert to numeric if fmt==NULL
  if (is.null(fmt)) {
    idx <- attr(dse, 'stub_width')
    for (i in (idx + 1):ncol(dse)) {
      dse[[i]] <- as.numeric(dse[[i]])
    }
  }

  # build
  out <- factory(dse,
    align = align,
    fmt = fmt,
    hrule = NULL,
    notes = notes,
    output = output,
    title = title,
    add_columns = add_columns,
    add_rows = add_rows)

  return(out)

}
