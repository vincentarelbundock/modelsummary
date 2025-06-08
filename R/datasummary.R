#' Summary tables using 2-sided formulae: crosstabs, frequencies, table
#' 1s and more.
#'
#' @description
#' `datasummary` can use any summary function which produces one numeric or
#' character value per variable. The examples section of this documentation
#' shows how to define custom summary functions.
#'
#' `modelsummary` also supplies several shortcut summary functions which can be used in `datasummary()` formulas: Min, Max, Mean, Median, Var, SD, NPercent, NUnique, Ncol, P0, P25, P50, P75, P100.
#'
#' See the Details and Examples sections below, and the vignettes on the `modelsummary` website:
#'
#' * https://modelsummary.com/
#' * https://modelsummary.com/vignettes/datasummary.html
#'
#' @template kableExtra2tinytable
#'
#' @inheritParams modelsummary
#' @import tables
#' @param formula A two-sided formula to describe the table: rows ~ columns.
#' See the Examples section for a mini-tutorial and the Details section for
#' more resources. Grouping/nesting variables can appear on both sides of the
#' formula, but all summary functions must be on one side.
#' @param data A data.frame (or tibble)
#' @param add_columns a data.frame (or tibble) with the same number of rows as
#' your main table.
#' @param fmt how to format numeric values: integer, user-supplied function, or `modelsummary` function.
#' * Integer: Number of decimal digits
#' * User-supplied functions:
#'   - Any function which accepts a numeric vector and returns a character vector of the same length.
#' * `modelsummary` functions:
#'   - `fmt = fmt_significant(2)`: Two significant digits (at the term-level)
#'   - `fmt = fmt_sprintf("%.3f")`: See `?sprintf`
#'   - `fmt = fmt_identity()`: unformatted raw values
#' @param sparse_header TRUE or FALSE. TRUE eliminates column headers which
#' have a unique label across all columns, except for the row immediately above
#' the data. FALSE keeps all headers. The order in which terms are entered in
#' the formula determines the order in which headers appear. For example,
#' `x~mean*z` will print the `mean`-related header above the `z`-related
#' header.`
#' @param ... all other arguments are passed through to the table-making
#' functions [tinytable::tt], [kableExtra::kbl], [gt::gt], [DT::datatable], etc. depending on the `output` argument.
#' This allows users to pass arguments directly to `datasummary` in order to
#' affect the behavior of other functions behind the scenes.
#' @template citation
#' @template options
#' @section Examples:
#'
#' ```{r, eval = identical(Sys.getenv("pkgdown"), "true")}
#' library(modelsummary)
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
#' #datasummary(hp + mpg ~ Mean + SD + Histogram, data = mtcars)
#'
#' # These functions also accept a 'fmt' argument which allows you to
#' # round/format the results
#'
#' datasummary(hp + mpg ~ Mean * Arguments(fmt = "%.3f") + SD * Arguments(fmt = "%.1f"), data = mtcars)
#'
#' # Save your tables to a variety of output formats:
#' f <- hp + mpg ~ Mean + SD
#' #datasummary(f, data = mtcars, output = 'table.html')
#' #datasummary(f, data = mtcars, output = 'table.tex')
#' #datasummary(f, data = mtcars, output = 'table.md')
#' #datasummary(f, data = mtcars, output = 'table.docx')
#' #datasummary(f, data = mtcars, output = 'table.pptx')
#' #datasummary(f, data = mtcars, output = 'table.jpg')
#' #datasummary(f, data = mtcars, output = 'table.png')
#'
#' # Display human-readable code
#' #datasummary(f, data = mtcars, output = 'html')
#' #datasummary(f, data = mtcars, output = 'markdown')
#' #datasummary(f, data = mtcars, output = 'latex')
#'
#' # Return a table object to customize using a table-making package
#' #datasummary(f, data = mtcars, output = 'tinytable')
#' #datasummary(f, data = mtcars, output = 'gt')
#' #datasummary(f, data = mtcars, output = 'kableExtra')
#' #datasummary(f, data = mtcars, output = 'flextable')
#' #datasummary(f, data = mtcars, output = 'huxtable')
#'
#' # add_rows
#' new_rows <- data.frame(a = 1:2, b = 2:3, c = 4:5)
#' attr(new_rows, 'position') <- c(1, 3)
#' datasummary(mpg + hp ~ mean + sd, data = mtcars, add_rows = new_rows)
#' ```
#'
#' @details
#' Visit the 'modelsummary' website for more usage examples:
#' https://modelsummary.com
#'
#' The 'datasummary' function is a thin wrapper around the 'tabular' function
#' from the 'tables' package. More details about table-making formulas can be
#' found in the 'tables' package documentation: ?tables::tabular
#'
#' Hierarchical or "nested" column labels are only available for these output
#' formats: tinytable, kableExtra, gt, html, rtf, and LaTeX. When saving tables to other
#' formats, nested labels will be combined to a "flat" header.
#' @export
datasummary <- function(
  formula,
  data,
  output = getOption("modelsummary_output", default = "default"),
  fmt = 2,
  title = getOption("modelsummary_title", default = NULL),
  notes = getOption("modelsummary_notes", default = NULL),
  align = getOption("modelsummary_align", default = NULL),
  add_columns = getOption("modelsummary_add_columns", default = NULL),
  add_rows = getOption("modelsummary_add_rows", default = NULL),
  sparse_header = getOption("modelsummary_sparse_header", default = TRUE),
  escape = getOption("modelsummary_escape", default = TRUE),
  ...
) {
  if (!isTRUE(list(...)[["internal_call"]])) {
    ## settings: don't overwrite settings on internal calls
    settings_init(
      settings = list(
        "function_called" = "datasummary"
      )
    )
  }

  tmp <- sanitize_output(output) # early
  output_format <- tmp$output_format
  output_factory <- tmp$output_factory
  output_file <- tmp$output_file
  sanitize_escape(escape) # after sanitize_output
  sanity_align(align)

  sanity_ds_data(
    formula = formula,
    data = data,
    internal_call = list(...)[["internal_call"]]
  )

  # convenience: transform logical and character to factor
  # are there use-cases for character variables?
  for (i in seq_along(data)) {
    if (is.character(data[[i]]) | is.logical(data[[i]])) {
      data[[i]] <- factor(data[[i]])
    }
  }

  # tibble -> data.frame (for All())
  data <- as.data.frame(data)

  # factor check
  sanity_ds_nesting_factor(formula, data)

  # create table
  tab <- tryCatch(tables::tabular(formula, data), error = function(e) e)

  # informative error message
  if (inherits(tab, "error")) {
    if (grepl("Duplicate values:", tab$message)) {
      message(
        'This error often occurs when the "*" nesting operator is used, but none of the nested terms are categorical variables (factor, logical or character types). You can transform your variable in the original data, or wrap it in a Factor() function in the `datasummary` formula.'
      )
    }
    stop(tab$message)
  }

  # extract content
  dse <- datasummary_extract(
    tab,
    fmt = fmt,
    sparse_header = sparse_header,
    data = data
  )

  # align stub l rest r
  stub_width <- attr(dse, "stub_width")
  tab_width <- ncol(dse)
  if (inherits(add_columns, "data.frame")) {
    tab_width <- tab_width + ncol(add_columns)
  }
  if (is.null(align)) {
    align <- paste0(
      strrep("l", stub_width),
      strrep("r", tab_width - stub_width)
    )
  }
  align <- paste(align, collapse = "")

  # convert to numeric if fmt==NULL
  if (is.null(fmt)) {
    idx <- attr(dse, "stub_width")
    for (i in (idx + 1):ncol(dse)) {
      dse[[i]] <- as.numeric(dse[[i]])
    }
  }

  # build
  out <- factory(
    dse,
    align = align,
    fmt = fmt,
    hrule = NULL,
    notes = notes,
    output = output,
    title = title,
    add_columns = add_columns,
    add_rows = add_rows,
    escape = escape,
    output_factory = output_factory,
    output_format = output_format,
    output_file = output_file,
    ...
  )

  # invisible return
  if (
    !is.null(output_file) ||
      output == "jupyter" ||
      (output == "default" && settings_equal("output_default", "jupyter"))
  ) {
    if (!isTRUE(list(...)[["internal_call"]])) settings_rm()
    return(invisible(out))
    # visible return
  } else {
    if (!isTRUE(list(...)[["internal_call"]])) settings_rm()
    return(out)
  }
}

#' `dsummary()` is a shortcut to `datasummary()`
#'
#' @inherit datasummary
#' @keywords internal
#' @export
dsummary <- datasummary
