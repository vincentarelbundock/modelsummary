#' Generate a correlation table for all numeric variables in your dataset.
#'
#' The names of the variables displayed in the correlation table are the names
#' of the columns in the `data`. You can rename those columns (with or without
#' spaces) to produce a table of human-readable variables.
#'
#' @inheritParams datasummary
#' @param method character or function
#' \itemize{
#'   \item character: "pearson", "kendall", "spearman", or "pearspear"
#'     (Pearson correlations above and Spearman correlations below the diagonal)
#'   \item function: takes a data.frame with numeric columns and returns a
#'     square matrix or data.frame with unique row.names and colnames
#'     corresponding to variable names. Note that the
#'     `datasummary_correlation_format` can often be useful for formatting the
#'     output of custom correlation functions.
#' }
#' @template options
#' @param ... other parameters are passed through to the table-making
#'     packages.
#' @export
#' @examples
#' \dontrun{
#' library(modelsummary)
#'
#' # clean variable names (base R)
#' dat <- mtcars[, c("mpg", "hp")]
#' colnames(dat) <- c("Miles / Gallon", "Horse Power")
#' datasummary_correlation(dat)
#'
#' # clean variable names (tidyverse)
#' library(tidyverse)
#' dat <- mtcars %>%
#'   select(`Miles / Gallon` = mpg,
#'          `Horse Power` = hp)
#' datasummary_correlation(dat)
#'
#' # alternative methods
#' datasummary_correlation(dat, method = "pearspear")
#'
#' # custom function
#' cor_fun <- function(x) cor(x, method = "kendall")
#' datasummary_correlation(dat, method = cor_fun)
#'
#' # rename columns alphabetically and include a footnote for reference
#' note <- sprintf("(%s) %s", letters[1:ncol(dat)], colnames(dat))
#' note <- paste(note, collapse = "; ")
#'
#' colnames(dat) <- sprintf("(%s)", letters[1:ncol(dat)])
#'
#' datasummary_correlation(dat, notes = note)
#'
#' # `datasummary_correlation_format`: custom function with formatting
#' dat <- mtcars[, c("mpg", "hp", "disp")]
#'
#' cor_fun <- function(x) {
#'   out <- cor(x, method = "kendall")
#'   datasummary_correlation_format(
#'     out,
#'     fmt = 2,
#'     upper_triangle = "x",
#'     diagonal = ".")
#' }
#'
#' datasummary_correlation(dat, method = cor_fun)
#'
#' # use kableExtra and psych to color significant cells
#' library(psych)
#' library(kableExtra)
#'
#' dat <- mtcars[, c("vs", "hp", "gear")]
#'
#' cor_fun <- function(dat) {
#'   # compute correlations and format them
#'   correlations <- data.frame(cor(dat))
#'   correlations <- datasummary_correlation_format(correlations, fmt = 2)
#'
#'   # calculate pvalues using the `psych` package
#'   pvalues <- psych::corr.test(dat)$p
#'
#'   # use `kableExtra::cell_spec` to color significant cells
#'   for (i in 1:nrow(correlations)) {
#'     for (j in 1:ncol(correlations)) {
#'       if (pvalues[i, j] < 0.05 && i != j) {
#'         correlations[i, j] <- cell_spec(correlations[i, j], background = "pink")
#'       }
#'     }
#'   }
#'   return(correlations)
#' }
#'
#' # The `escape=FALSE` is important here!
#' datasummary_correlation(dat, method = cor_fun, escape = FALSE)
#' }
datasummary_correlation <- function(data,
                                    output = 'default',
                                    method = "pearson",
                                    fmt = 2,
                                    align = NULL,
                                    add_rows = NULL,
                                    add_columns = NULL,
                                    title = NULL,
                                    notes = NULL,
                                    escape = TRUE,
                                    ...) {


  ## settings
  settings_init(settings = list(
    "function_called" = "datasummary_correlation"
  ))

  # sanity checks
  sanitize_output(output)
  sanitize_escape(escape)
  sanity_add_columns(add_columns)

  any_numeric <- any(sapply(data, is.numeric) == TRUE)
  if (any_numeric == FALSE) {
    stop("`datasummary_correlation` can only summarize numeric data columns.")
  }

  checkmate::assert(
    checkmate::check_choice(
      method,
      c("pearson", "kendall", "spearman", "pearspear")),
    checkmate::check_function(method))

  # assign correlation computation function
  if (is.function(method)) {
    fn <- method
  } else if (method == "pearspear") {
    fn <- correlation_pearspear
  } else {
    fn <- function(x) stats::cor(
      x,
      use = "pairwise.complete.obs",
      method = method)
  }

  # subset numeric and compute correlation
  out <- data[, sapply(data, is.numeric), drop = FALSE]
  out <- fn(out)

  if ((!is.matrix(out) && !inherits(out, "data.frame")) ||
      is.null(row.names(out)) ||
      is.null(colnames(out)) ||
      nrow(out) != ncol(out)) {
    stop("The function supplied to the `method` argument did not return a square matrix or data.frame with row.names and colnames.")
  }

  if (is.character(method)) {
    if (method == "pearspear") {
      out <- datasummary_correlation_format(
        out,
        fmt = fmt,
        diagonal = "1")
    } else {
      out <- datasummary_correlation_format(
        out,
        fmt = fmt,
        diagonal = "1",
        upper_triangle = ".")
    }
  } else {
    out <- datasummary_correlation_format(
      out,
      fmt = fmt)
  }

  col_names <- colnames(out)
  out <- cbind(rowname = row.names(out), out)
  colnames(out) <- c(' ', col_names)

  if (is.null(align)) {
      ncols <- ncol(out)
      if (!is.null(add_columns)) {
          ncols <- ncols + ncol(add_columns)
      }
      align <- paste0('l', strrep('r', ncols - 1))
  }

  if (settings_equal("escape", TRUE)) {
    out[, 1] <- escape_string(out[, 1])
    colnames(out) <- escape_string(colnames(out))
  }

  out <- factory(out,
    align = align,
    hrule = NULL,
    output = output,
    add_rows = add_rows,
    add_columns = add_columns,
    notes = notes,
    title = title,
    ...)

  if (!is.null(settings_get("output_file"))) {
    settings_rm()
    return(invisible(out))
  } else {
    settings_rm()
    return(out)
  }

}

correlation_pearspear <- function(x) {
  pea <- stats::cor(
    x,
    use = "pairwise.complete.obs",
    method = "pearson"
  )

  spe <- stats::cor(
    x,
    use = "pairwise.complete.obs",
    method = "spearman"
  )

  pea[lower.tri(pea)] <- spe[lower.tri(spe)]

  return(pea)
}


#' Format the content of a correlation table
#'
#' Mostly for internal use, but can be useful when users supply a function to
#' the `method` argument of `datasummary_correlation`.
#' @inheritParams datasummary_correlation
#' @param x square numeric matrix
#' @param leading_zero boolean. If FALSE, leading zeros are removed
#' @param diagonal character or NULL. If character, all elements of the
#' diagonal are replaced by the same character (e.g., "1").
#' @param upper_triangle character or NULL. If character, all elements of the
#' upper triangle are replaced by the same character (e.g., "" or ".").
#' @export
#' @examples
#' \dontrun{
#' library(modelsummary)
#'
#' dat <- mtcars[, c("mpg", "hp", "disp")]
#'
#' cor_fun <- function(x) {
#'   out <- cor(x, method = "kendall")
#'   datasummary_correlation_format(
#'     out,
#'     fmt = 2,
#'     upper_triangle = "x",
#'     diagonal = ".")
#' }
#'
#' datasummary_correlation(dat, method = cor_fun)
#' }
datasummary_correlation_format <- function(
  x,
  fmt,
  leading_zero = FALSE,
  diagonal = NULL,
  upper_triangle = NULL) {

  # sanity
  checkmate::assert_character(diagonal, len = 1, null.ok = TRUE)
  checkmate::assert_character(upper_triangle, len = 1, null.ok = TRUE)
  checkmate::assert_flag(leading_zero)

  out <- data.frame(x, check.names = FALSE)

  for (i in seq_along(out)) {
    out[[i]] <- rounding(out[[i]], fmt)
    if (leading_zero == FALSE) {
      out[[i]] <- gsub('0\\.', '\\.', out[[i]])
    }
  }

  for (i in 1:nrow(out)) {
    for (j in 1:ncol(out)) {
      if (!is.null(upper_triangle)) {
        out[i, j] <- ifelse(i < j, upper_triangle, out[i, j])
      }
      if (!is.null(diagonal)) {
        out[i, j] <- ifelse(i == j, diagonal, out[i, j])
      }
    }
  }

  return(out)
}
