#' Generate a correlation table for all numeric variables in your dataset.
#'
#' The names of the variables displayed in the correlation table are the names
#' of the columns in the `data`. You can rename those columns (with or without
#' spaces) to produce a table of human-readable variables. See the Details and
#' Examples sections below, and the vignettes on the `modelsummary` website:
#' * https://modelsummary.com/
#' * https://modelsummary.com/vignettes/datasummary.html
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
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
#' @template kableExtra2tinytable
#' @template citation
#' @template options
#' @param ... other parameters are passed through to the table-making
#'     packages.
#' @export
#' @section Examples:
#' ```{r, eval = identical(Sys.getenv("pkgdown"), "true")}
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
#' # `correlation` package objects
#' if (requireNamespace("correlation", quietly = TRUE)) {
#'   co <- correlation::correlation(mtcars[, 1:4])
#'   datasummary_correlation(co)
#'
#'   # add stars to easycorrelation objects
#'   datasummary_correlation(co, stars = TRUE)
#' }
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
#' ```
datasummary_correlation <- function(
  data,
  output = getOption("modelsummary_output", default = "default"),
  method = getOption("modelsummary_method", default = "pearson"),
  fmt = 2,
  align = getOption("modelsummary_align", default = NULL),
  add_rows = getOption("modelsummary_add_rows", default = NULL),
  add_columns = getOption("modelsummary_add_columns", default = NULL),
  title = getOption("modelsummary_title", default = NULL),
  notes = getOption("modelsummary_notes", default = NULL),
  escape = getOption("modelsummary_escape", default = TRUE),
  stars = getOption("modelsummary_stars", default = FALSE),
  ...
) {
  ## settings
  settings_init(
    settings = list(
      "function_called" = "datasummary_correlation"
    )
  )

  # sanity checks
  tmp <- sanitize_output(output) # before sanitize_escape
  output_format <- tmp$output_format
  output_factory <- tmp$output_factory
  output_file <- tmp$output_file
  sanitize_escape(escape) # after sanitize_output
  sanity_add_columns(add_columns)
  sanity_align(align)

  if (inherits(data, "data.table")) {
    data <- as.data.frame(data, check.names = FALSE)
  }

  easycorrelation <- inherits(data, "easycorrelation")

  if (isFALSE(easycorrelation) && !isFALSE(stars)) {
    msg <- "The `stars` argument of the `datasummary_correlation()` function is only supported when `x` is an object produced by the `correlation` package."
    insight::format_error(msg)
  }

  if (easycorrelation) {
    insight::check_if_installed("correlation")
    easycorrelation <- TRUE
    s <- summary(data, redundant = TRUE)
    data <- as.matrix(data)
    data <- as.data.frame(data)
    # store the p values in a "attribute" of the object
    # this is retrieved and used in the `_format()` function.
    attr(data, "p") <- attr(s, "p")
  }

  any_numeric <- any(sapply(data, is.numeric) == TRUE)
  if (any_numeric == FALSE) {
    stop("`datasummary_correlation` can only summarize numeric data columns.")
  }

  checkmate::assert(
    checkmate::check_choice(
      method,
      c("pearson", "kendall", "spearman", "pearspear")
    ),
    checkmate::check_function(method)
  )

  # assign correlation computation function
  if (is.function(method)) {
    fn <- method
  } else if (method == "pearspear") {
    fn <- correlation_pearspear
  } else {
    fn <- function(x) {
      stats::cor(
        x,
        use = "pairwise.complete.obs",
        method = method
      )
    }
  }

  # subset numeric and compute correlation
  if (easycorrelation == FALSE) {
    out <- data.frame(data, check.names = FALSE) # data.table & tibble
    out <- data[, sapply(data, is.numeric), drop = FALSE]
    out <- fn(out)
  } else {
    out <- data
  }

  if (
    (!is.matrix(out) && !inherits(out, "data.frame")) ||
      is.null(row.names(out)) ||
      is.null(colnames(out)) ||
      nrow(out) != ncol(out)
  ) {
    stop(
      "The function supplied to the `method` argument did not return a square matrix or data.frame with row.names and colnames."
    )
  }

  if (easycorrelation) {
    out <- datasummary_correlation_format(
      out,
      fmt = fmt,
      diagonal = "1",
      upper_triangle = ".",
      stars = stars
    )
  } else if (is.character(method)) {
    if (method == "pearspear") {
      out <- datasummary_correlation_format(
        out,
        fmt = fmt,
        diagonal = "1"
      )
    } else {
      out <- datasummary_correlation_format(
        out,
        fmt = fmt,
        diagonal = "1",
        upper_triangle = "."
      )
    }
  } else {
    out <- datasummary_correlation_format(
      out,
      fmt = fmt
    )
  }

  col_names <- colnames(out)
  out <- cbind(rowname = row.names(out), out)
  colnames(out) <- c(" ", col_names)

  if (is.null(align)) {
    ncols <- ncol(out)
    if (!is.null(add_columns)) {
      ncols <- ncols + ncol(add_columns)
    }
    align <- paste0("l", strrep("r", ncols - 1))
  }

  # labelled data
  dict <- get_variable_labels_data(data)
  out[, 1] <- replace_dict(out[, 1], dict)
  colnames(out) <- replace_dict(colnames(out), dict)

  out <- factory(
    out,
    align = align,
    hrule = NULL,
    output = output,
    add_rows = add_rows,
    add_columns = add_columns,
    notes = notes,
    title = title,
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
    settings_rm()
    return(invisible(out))
    # visible return
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
#' @param leading_zero boolean. If `FALSE`, leading zeros are removed
#' @param diagonal character or NULL. If character, all elements of the
#' diagonal are replaced by the same character (e.g., "1").
#' @param upper_triangle character or NULL. If character, all elements of the
#' upper triangle are replaced by the same character (e.g., "" or ".").
#' @export
#' @examples
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
datasummary_correlation_format <- function(
  x,
  fmt,
  leading_zero = FALSE,
  diagonal = NULL,
  upper_triangle = NULL,
  stars = FALSE
) {
  # sanity
  checkmate::assert_character(diagonal, len = 1, null.ok = TRUE)
  checkmate::assert_character(upper_triangle, len = 1, null.ok = TRUE)
  checkmate::assert_flag(leading_zero)

  p <- attr(x, "p")

  out <- data.frame(x, check.names = FALSE)

  for (i in seq_along(out)) {
    fmt <- sanitize_fmt(fmt)
    out[[i]] <- fmt(out[[i]])
    if (leading_zero == FALSE) {
      out[[i]] <- gsub("0\\.", "\\.", out[[i]])
    }
  }

  # before triangle, otherwise we get empty cells with stars
  if (!is.null(p) && !isFALSE(stars)) {
    st <- make_stars(p, stars)
    st <- st[, 2:ncol(st)]
    for (j in 1:ncol(out)) {
      out[, j] <- paste0(out[, j], st[, j])
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
