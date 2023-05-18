#' datasummary statistic shortcut
#'
#' @keywords internal
#' @export
#' @param x variable to summarize
#' @param fmt passed to the `modelsummary:::rounding` function
#' @param na.rm  a logical value indicating whether ‘NA’ values should be
#'   stripped before the computation proceeds.
#' @param ... unused
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
Mean <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- mean(x, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}


#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(Factor(cyl) ~ N, data = mtcars)
#' }
N <- function(x) {
  # number of elements in a subgroup (factor)
  if (is.logical(x) && all(is.na(x))) {
    sprintf("%.0f", length(x))
  # number of non-missing observations
  } else {
    sprintf("%.0f", sum(!is.na(x)))
  }
}


#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
Median <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::median(x, na.rm = na.rm)
  if (!is.null(fmt)) {
    fmt <- sanitize_fmt(fmt)
    out <- fmt(out)
  }
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
Min <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- min(x, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}


#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
Max <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- max(x, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
SD <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::sd(x, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
Var <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::var(x, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
Ncol <- function(x, ...)
  paste0('(N = ', length(x), ')')

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @param y denominator variable
#' @export
NPercent <- function(x, y) {
  pct <- round(100 * length(x) / length(y))
  n <- length(x)
  if (n == 0) {
    out <- '.'
  } else {
    out <- paste0(n, ' (', pct, '%)')
  }
  return(out)
}


#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(cyl + hp ~ NUnique, data = mtcars)
#' }
NUnique <- function(x, ...) sprintf("%.0f", length(unique(x)))


#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
P0 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::quantile(x, prob = 0, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
P25 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::quantile(x, prob = 0.25, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
P50 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::quantile(x, prob = 0.50, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
P75 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::quantile(x, prob = 0.75, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
#' @examples
#' if (identical(Sys.getenv("pkgdown"),  "true")) {
#' datasummary(mpg + hp ~ Mean + Median + P0 + P25 + P50 + P75 + P100 +
#'             Min + Max + SD + Var,
#'             data = mtcars)
#' }
P100 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
  out <- stats::quantile(x, prob = 1, na.rm = na.rm)
  out <- sanitize_fmt(fmt)(out, ...)
  return(out)
}

#' datasummary statistic shortcut
#' @keywords internal
#' @inheritParams Mean
#' @export
PercentMissing <- function(x) {
  sprintf("%.0f", mean(is.na(x)) * 100)
}

#' datasummary statistic shortcut
#'
#' This function uses Unicode characters to create a histogram. This can
#' sometimes be useful, but is generally discouraged. Unicode characters can
#' only display a limited number of heights for bars, and the accuracy of
#' output is highly dependent on the platform (typeface, output type, windows
#' vs. mac, etc.). We recommend you use the `kableExtra::spec_hist` function
#' instead.
#'
#' @keywords internal
#' @inheritParams Mean
#' @param bins number of histogram bars
#' @export
Histogram <- function(x, bins = 10) {
  # ticks <- c(" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█")
  ticks <- c(" ", "\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588")
  barheight <- cut(x, breaks = bins, labels = FALSE)
  barheight <- table(barheight)
  barheight <- barheight / max(barheight) * 7 + 1
  barheight <- round(barheight)
  bars <- ticks[barheight]
  out <- paste(bars, collapse = '')
  return(out)
}
