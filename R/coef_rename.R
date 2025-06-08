#' Rename model terms
#'
#' A convenience function which can be passed to the `coef_rename` argument of
#' the `modelsummary` function.
#'
#' @param x character vector of term names to transform
#' @param factor boolean remove the "factor()" label
#' @param factor_name boolean remove the "factor()" label and the name of the
#' variable
#' @param poly boolean remove the "poly()" label and function arguments
#' @param backticks boolean remove backticks
#' @param titlecase boolean convert to title case
#' @param underscore boolean replace underscores by spaces
#' @param asis boolean remove the `I` from as-is formula calls
#'
#' @export
#' @section Examples:
#' ```{r, eval = identical(Sys.getenv("pkgdown"), "true")}
#' library(modelsummary)
#' dat <- mtcars
#' dat$horse_power <- dat$hp
#' mod <- lm(mpg ~ horse_power + factor(cyl), dat)
#' modelsummary(mod, coef_rename = coef_rename)
#' ```
coef_rename <- function(
  x,
  factor = TRUE,
  factor_name = TRUE,
  poly = TRUE,
  backticks = TRUE,
  titlecase = TRUE,
  underscore = TRUE,
  asis = TRUE
) {
  out <- x

  if (isTRUE(factor_name)) {
    out <- gsub("factor\\(.*\\)", "", out)
  } else if (isTRUE(factor)) {
    out <- gsub("factor\\((.*)\\)", "\\1 ", out)
  }

  if (isTRUE(poly)) {
    out <- gsub("poly\\(([^,]*),.*\\)", "\\1^", out)
  }

  if (isTRUE(underscore)) {
    out <- gsub("_", " ", out)
  }

  if (isTRUE(backticks)) {
    out <- gsub("\\`", "", out)
  }

  if (isTRUE(asis)) {
    out <- gsub("\\bI\\((.*)\\)", "(\\1)", out)
  }

  if (isTRUE(titlecase)) {
    out <- tools::toTitleCase(out)
  }

  return(out)
}
