#' Quick overview of numeric or categorical variables
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @param histogram TRUE includes a histogram for numeric variables (boolean). This option is
#' only supported for HTML and LaTeX tables. The examples section shows one way
#' to reproduce the summary with histogram in other formats.
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
#' Note that the `Histogram` function relies on unicode characters. These
#' characters will only display correctly in some operating systems, under some
#' locales, using some fonts. The `modelsummary` authors cannot provide support
#' to display these unicode histograms.
#'
#' f <- All(dat, numeric = TRUE, factor = FALSE) ~
#'      (Mean + SD + Min + Median + Max) * Arguments(fmt = fmt) +
#'      Heading("") * Histogram
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

  checkmate::assert_true(type %in% c("numeric", "categorical"))

  # tables does not play well with tibbles
  data <- as.data.frame(data)

  # output format
  output_info <- modelsummary:::parse_output_arg(output)

  if (type == 'numeric') {

    # draw histogram?
    if (histogram) {
      histogram_col <- function(x) ""
      if ((output_info$output_format %in% c("html", "kableExtra")) || knitr::is_latex_output()) {
        histogram <- TRUE
      } else {
        histogram <- FALSE
        warning('The histogram option is only available for output of types "html" and "kableExtra", and for PDF and HTML documents produced by Rmarkdown or knitr. Use `histogram=FALSE` to silence this warning.')
      }
    }

    # subset of numeric variables with non NA values
    dat_new <- data[, sapply(data, is.numeric), drop=FALSE] 
    dat_new <- dat_new[, sapply(dat_new, function(x) !all(is.na(x))), drop=FALSE]

    if (ncol(dat_new) == 0) {
      stop('data contains no numeric variable.')
    }

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

    # logical | character -> factor for unit breakdowns
    dropped <- FALSE
    for (n in colnames(dat_new)) {
      if (is.logical(dat_new[[n]]) || is.character(dat_new[[n]])) {
        dat_new[[n]] <- as.factor(dat_new[[n]])
        if (length(levels(dat_new[[n]])) > 10) {
          dropped <- TRUE
          dat_new[[n]] <- NULL
        }
      }
    }

    if (dropped) {
      warning("datasummary_skim dropped a categorical variable because it contained more than 10 levels.")
    }

    # subset of categorical variables with non-NA values
    idx <- sapply(dat_new, is.factor) && sapply(dat_new, function(x) !all(is.na(x)))
    dat_new <- dat_new[, idx, drop=FALSE] 

    if (ncol(dat_new) == 0) {
      stop('data contains no logical, character, or factor variable.')
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
