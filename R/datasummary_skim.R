#' Quick overview of numeric or categorical variables
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @param histogram TRUE to include a SVG histogram (boolean). This option is
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

  # tables does not play well with tibbles
  data <- as.data.frame(data)

  # output format
  out <- parse_output_arg(output)

  # draw histogram?
  if (histogram) {
    histogram_col <- function(x) ""
    histogram <- out$output_format %in% c("latex", "html", "kableExtra") &&
                 out$output_factory == "kableExtra"
  }

  if (type == 'numeric') {

    # subset of numeric variables with non NA values
    dat_new <- data[, sapply(data, is.numeric)] 
    dat_new <- dat_new[, sapply(dat_new, function(x) !all(is.na(x)))]

    if (ncol(dat_new) == 0) {
      stop('data contains no numeric variable.')
    }

    # with histogram
    if (histogram) {
      f <- All(dat_new, numeric = TRUE, factor = FALSE) ~
           (Mean + SD + Min + Median + Max) * Arguments(fmt = fmt) +
           Heading("") * histogram_col

      # prepare histogram
      idx <- datasummary(f, fmt="%.2f", data=dat_new, output="data.frame")[[1]]
      histograms <- as.list(data[, idx])
      histograms <- lapply(histograms, scale)
      histograms <- lapply(histograms, as.numeric)
      histograms <- lapply(histograms, na.omit)

      # draw table
      out <- datasummary(formula = f,
          data = dat_new,
          output = output,
          title = title,
          align = align,
          notes = notes) %>%
        kableExtra::column_spec(
          column=7, 
          image=kableExtra::spec_hist(histograms, col="black")
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

    # subset of categorical variables with non-NA values
    is.categorical <- function(x) {
      is.character(x) || is.factor(x) || is.logical(x)
    }
    dat_new <- data[, sapply(data, is.categorical)] 
    dat_new <- dat_new[, sapply(dat_new, function(x) !all(is.na(x)))]

    for (n in colnames(dat_new)) {
      if (is.logical(dat_new[[n]])) {
        dat_new[[n]] <- as.factor(dat_new[[n]])
      }
    }
    if (ncol(dat_new) == 0) {
      stop('data contains no logical, character, or factor variable.')
    }

    pctformat = function(x) sprintf("%.1f", x)
    f <- All(dat_new, numeric=FALSE, factor=TRUE, logical=TRUE) ~
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
