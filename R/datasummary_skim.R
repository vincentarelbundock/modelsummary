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

  checkmate::assert_true(type %in% c("numeric", "categorical"))

  # tables does not play well with tibbles
  data <- as.data.frame(data)

  # output format
  output_info <- modelsummary:::parse_output_arg(output)

  if (type == 'numeric') {

    # draw histogram?
    if (histogram) {
      histogram_col <- function(x) ""

      good <- c("html", "png", "jpg", "kableExtra", "default")
      if ((output_info$output_format %in% good) || knitr::is_latex_output()) {
        histogram <- TRUE

      } else {
        histogram <- FALSE

        warning('The histogram argument is only supported for these output types: "default", "html", "jpg", "png", and "kableExtra". It also works in Rmarkdown/knitr documents compiled to PDF or HTML. Use `histogram=FALSE` to silence this warning.')

      }
    }

    # subset of numeric variables with non NA values
    dat_new <- data[, sapply(data, is.numeric), drop=FALSE] 
    dat_new <- dat_new[, sapply(dat_new, function(x) !all(is.na(x))), drop=FALSE]

    if (ncol(dat_new) == 0) {
      stop('data contains no numeric variable.')
    }

    # pad colnames in case one is named Min, Max, Mean, or other function name
    colnames(dat_new) <- paste0(colnames(dat_new), " ")


    # with histogram
    if (histogram) {
      f <- All(dat_new, numeric=TRUE, factor=FALSE) ~
           (Mean + SD + Min + Median + Max) * Arguments(fmt = fmt) +
           Heading("") * histogram_col

      # prepare list of histograms
      # TODO: inefficient because it computes the table twice. But I need to
      # know the exact subset of variables kept by tabular, in the exact
      # order, to print the right histograms.

      idx <- datasummary(f, data=dat_new, output="data.frame")[[1]]
      histogram_list <- as.list(data[, idx, drop=FALSE])
      for (n in names(histogram_list)) {
        histogram_list[[n]] <- as.numeric(scale(stats::na.omit(histogram_list[[n]])))
        if (all(is.nan(histogram_list[[n]]))) {
          histogram_list[[n]] <- 0
        }
      }

      # too large
      if (ncol(dat_new) > 50) {
        stop("Cannot summarize more than 50 variables at a time.")
      }

      # draw table
      out <- datasummary(formula = f,
          data = dat_new,
          output = output,
          title = title,
          align = align,
          notes = notes) %>%
        kableExtra::column_spec(
          column=7, 
          image=kableExtra::spec_hist(histogram_list, 
                                      col="black")
        )

    # without histogram
    } else {

      f <- All(dat_new, numeric = TRUE, factor = FALSE) ~
           (Mean + SD + Min + Median + Max) * Arguments(fmt = fmt)
      out <- datasummary(f,
          data = dat_new,
          output = output,
          title = title,
          align = align,
          notes = notes) 

    }

  } else if (type == 'categorical') {

    dat_new <- data

    # pad colnames in case one is named Min, Max, Mean, or other function name
    colnames(dat_new) <- paste0(colnames(dat_new), " ")

    for (n in colnames(dat_new)) {

      if (is.logical(dat_new[[n]]) | 
          is.character(dat_new[[n]]) | 
          is.factor(dat_new[[n]])) {

        # convert to factor
        dat_new[[n]] <- factor(dat_new[[n]])

        # pad factor levels, otherwise tables::tabular breaks on ""
        if (is.factor(dat_new[[n]])) {
          levels(dat_new[[n]]) <- paste0(dat_new[[n]], " ")
        }

        # factors with too many levels
        if (is.factor(dat_new[[n]])) {
          if (length(levels(dat_new[[n]])) > 20) {
            dat_new[[n]] <- NULL
            warning("datasummary_skim dropped a categorical variable because it contained more than 20 levels.")
          }
        }

        # drop completely missing
        if (all(is.na(dat_new[[n]]))) {
          dat_new[[n]] <- NULL
          warning("datasummary_skim dropped a categorical variable because it was entirely NA.")
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

    pctformat = function(x) sprintf("%.1f", x)
    f <- All(dat_new, numeric=FALSE, factor=TRUE, logical=TRUE, character=TRUE) ~
         (N = 1) * Format(digits=0) + (`%` = Percent()) * Format(pctformat())

    out <- datasummary(
      formula = f,
      data = dat_new,
      output = output,
      title = title,
      align = align,
      notes = notes) 

  } 

  out

}
