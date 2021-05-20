#' Quick overview of numeric or categorical variables
#'
#' This function was inspired by the excellent `skimr` package for R.
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @param histogram include a histogram (TRUE/FALSE). Supported for:
#' \itemize{
#' \item type = "numeric"
#' \item output is "html", "default", "jpg", "png", or "kableExtra"
#' \item PDF and HTML documents compiled via Rmarkdown or knitr
#' \item See the examples section below for an example of how to use
#' `datasummary` to include histograms in other formats such as markdown.
#' }
#' @param type of variables to summarize: "numeric" or "categorical" (character)
#'
#' @examples
#'
#' \dontrun{
#' dat <- mtcars
#' dat$vs <- as.logical(dat$vs)
#' dat$cyl <- as.factor(dat$vs)
#' datasummary_skim(dat)
#' datasummary_skim(dat, "categorical")
#'
#' # You can use `datasummary` to produce a similar table in different formats.
#' # Note that the `Histogram` function relies on unicode characters. These
#' # characters will only display correctly in some operating systems, under some
#' # locales, using some fonts. Displaying such histograms on Windows computers
#' # is notoriously tricky. The `modelsummary` authors cannot provide support to
#' # display these unicode histograms.
#'
#' f <- All(mtcars) ~ Mean + SD + Min + Median + Max + Histogram
#' datasummary(f, mtcars, output="markdown")
#'
#' }
#' @export
datasummary_skim <- function(data,
                             type   = 'numeric',
                             output = 'default',
                             fmt    = '%.1f',
                             histogram = TRUE,
                             title  = NULL,
                             notes  = NULL,
                             align  = NULL,
                             ...) {

  sanity_output(output)

  checkmate::assert_true(type %in% c("numeric", "categorical", "dataset"))

  # tables does not play well with tibbles
  data <- as.data.frame(data)

  if (type == "numeric") {
    out <- datasummary_skim_numeric(data, output = output, fmt = fmt,
                                    histogram = histogram, title = title,
                                    notes = notes, align = align, ...)
  }

  if (type == "categorical") {
    out <- datasummary_skim_categorical(data, output = output, fmt = fmt,
                                        title = title, notes = notes, align = align,
                                        ...)
  }

  if (type == "dataset") {
    out <- datasummary_skim_dataset(data, output = output, title = title,
                                    notes = notes, align = align, ...)
  }

  return(out)

}

#' Internal function to skim whole datasets
#'
#' @noRd
datasummary_skim_dataset <- function(
  data,
  output,
  title,
  notes,
  align,
  ...) {


  is.binary <- function(x) {
    tryCatch(length(unique(stats::na.omit(x))) == 2, error = function(e) FALSE, silent = TRUE)
  }
  out <- c(
    Rows = rounding(nrow(data), fmt = 0),
    Columns = rounding(ncol(data), fmt = 0),
    # `# Binary` = rounding(sum(sapply(data, is.binary)), 0),
    `# Character` = rounding(sum(sapply(data, is.character)), 0),
    `# Factor` = rounding(sum(sapply(data, is.factor)), 0),
    `# Logical` = rounding(sum(sapply(data, is.logical)), 0),
    `# Numeric` = rounding(sum(sapply(data, is.numeric)), 0),
    `% Missing` = rounding(mean(is.na(data) * 100), 0)
  )
  out <- data.frame(names(out), out)
  out <- out[out[[2]] != "0" | out[[1]] == "% Missing", ]
  row.names(out) <- NULL
  colnames(out) <- c(" ", "  ")

  out <- datasummary_df(
    data = out,
    output = output,
    title = title,
    align = align,
    notes = notes,
    ...)

  return(out)

}


#' Internal function to skim numeric variables
#'
#' @noRd
datasummary_skim_numeric <- function(
  data,
  output,
  fmt,
  histogram,
  title,
  notes,
  align,
  ...) {

  # output format
  output_info <- sanitize_output(output)

  # draw histogram?
  if (histogram) {

    # histogram is a kableExtra-specific option
    if (output_info$output_factory != "kableExtra") {
      histogram <- FALSE
    }

    # write to file
    if (!is.null(output_info$output_file)) {
      if (!output_info$output_format %in% c("html", "png", "jpg")) {
        histogram <- FALSE
      }

    # interactive or Rmarkdown/knitr
    } else {
      if (check_dependency("knitr")) {
        if (!output_info$output_format %in% c("default", "html", "kableExtra") &&
            !knitr::is_latex_output()) {
          histogram <- FALSE
        }
      } else {
        if (!output_info$output_format %in% c("default", "html", "kableExtra")) {
          histogram <- FALSE
        }
      }
    }

    # if flag was flipped
    if (!histogram) {
      warning('The histogram argument is only supported for (a) output types
              "default", "html", or "kableExtra"; (b) writing to file paths
              with extensions ".html", ".jpg", or ".png"; and (c) Rmarkdown
              or knitr documents compiled to PDF or HTML. Use
              `histogram=FALSE` to silence this warning.')
    }

  }

  # subset of numeric variables
  idx <- sapply(data, is.numeric)
  if (!any(idx)) stop('data contains no numeric variable.')
  dat_new <- data[, idx, drop = FALSE]

  # subset of non-NA variables
  idx <- sapply(dat_new, function(x) !all(is.na(x)))
  if (!any(idx)) stop('all numeric variables are completely missing.')
  dat_new <- dat_new[, idx, drop = FALSE]

  # convert to numeric (tables does not play well with haven_labelled)
  for (i in seq_along(dat_new)) {
    dat_new[[i]] <- as.numeric(dat_new[[i]])
  }

  # pad colnames in case one is named Min, Max, Mean, or other function name
  # colnames(dat_new) <- paste0(colnames(dat_new), " ")

  # with histogram
  if (histogram) {

    histogram_col <- function(x) ""

    f <- All(dat_new, numeric = TRUE, factor = FALSE) ~
         Heading("Unique (#)") * NUnique +
         Heading("Missing (%)") * PercentMissing +
         (Mean + SD + Min + Median + Max) * Arguments(fmt = fmt) +
         Heading("") * histogram_col

    # prepare list of histograms
    # TODO: inefficient because it computes the table twice. But I need to
    # know the exact subset of variables kept by tabular, in the exact
    # order, to print the right histograms.

    idx <- datasummary(f, data = dat_new, output = "data.frame")[[1]]
    histogram_list <- as.list(dat_new[, idx, drop = FALSE])
    histogram_list <- lapply(histogram_list, stats::na.omit)

    # too large
    if (ncol(dat_new) > 50) {
      stop("Cannot summarize more than 50 variables at a time.")
    }

    # don't use output=filepath.html when post-processing
    if (!is.null(output_info$output_file)) {
      output <- "kableExtra"
    }

    # draw table
    out <- datasummary(formula = f,
        data = dat_new,
        output = output,
        title = title,
        align = align,
        notes = notes)
    out <- kableExtra::column_spec(out,
        column = 9,
        image = kableExtra::spec_hist(histogram_list,
                                      col = "black",
                                      same_lim = FALSE)
      )

    # don't use output=filepath.html when post-processing
    if (!is.null(output_info$output_file)) {
      kableExtra::save_kable(out, file = output_info$output_file)
      return(invisible(out))
    }

  # without histogram
  } else {

    f <- All(dat_new, numeric = TRUE, factor = FALSE) ~
         Heading("Unique (#)") * NUnique +
         Heading("Missing (%)") * PercentMissing +
         (Mean + SD + Min + Median + Max) * Arguments(fmt = fmt)

    out <- datasummary(f,
        data = dat_new,
        output = output,
        title = title,
        align = align,
        notes = notes)

  }

  return(out)

}


#' Internal function to skim categorical variables
#'
#' @noRd
datasummary_skim_categorical <- function(
  data,
  output,
  fmt,
  title,
  notes,
  align,
  ...) {

  dat_new <- data

  # pad colnames in case one is named Min, Max, Mean, or other function name
  # colnames(dat_new) <- paste0(colnames(dat_new), " ")

  drop_too_many_levels <- NULL
  drop_entirely_na <- NULL

  for (n in colnames(dat_new)) {

    if (is.logical(dat_new[[n]]) |
        is.character(dat_new[[n]]) |
        is.factor(dat_new[[n]])) {

      # convert to factor
      dat_new[[n]] <- factor(dat_new[[n]])

      # tables::tabular breaks on ""
      if (is.factor(dat_new[[n]])) {
        levels(dat_new[[n]])[levels(dat_new[[n]]) == ""] <- " "
      }

      # completely missing
      if (all(is.na(dat_new[[n]]))) {
        dat_new[[n]] <- NULL
        drop_entirely_na <- c(drop_entirely_na, n)
      } else {
        # factors with too many levels
        if (is.factor(dat_new[[n]])) {
          if (length(levels(dat_new[[n]])) > 50) {
            dat_new[[n]] <- NULL
            drop_too_many_levels <- c(drop_too_many_levels, n)
          }
        }
      }


    # discard non-factors
    } else {
      dat_new[[n]] <- NULL
    }

  }

  # too small
  if (ncol(dat_new) == 0) {
    stop('data contains no logical, character, or factor variable.')
  }

  # too large
  if (ncol(dat_new) > 50) {
    stop("Cannot summarize more than 50 variables at a time.")
  }

  if (!is.null(drop_too_many_levels)) {
    warning(sprintf("These variables were omitted because they include more than 50 levels: %s.", paste(drop_too_many_levels, collapse=", ")))
  }

  if (!is.null(drop_entirely_na)) {
    warning(sprintf("These variables were omitted because they are entirely missing: %s.", paste(drop_entirely_na, collapse=", ")))
  }

  pctformat <- function(x) rounding(x, fmt)
  f <- All(dat_new, numeric = FALSE, factor = TRUE, logical = TRUE, character = TRUE) ~
       (N = 1) * Format() + (`%` = Percent()) * Format(pctformat())

  out <- datasummary(
    formula = f,
    data = dat_new,
    output = output,
    title = title,
    align = align,
    notes = notes)

  return(out)

}
