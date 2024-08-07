% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasummary_correlation.R
\name{datasummary_correlation_format}
\alias{datasummary_correlation_format}
\title{Format the content of a correlation table}
\usage{
datasummary_correlation_format(
  x,
  fmt,
  leading_zero = FALSE,
  diagonal = NULL,
  upper_triangle = NULL,
  stars = FALSE
)
}
\arguments{
\item{x}{square numeric matrix}

\item{fmt}{how to format numeric values: integer, user-supplied function, or \code{modelsummary} function.
\itemize{
\item Integer: Number of decimal digits
\item User-supplied functions:
\itemize{
\item Any function which accepts a numeric vector and returns a character vector of the same length.
}
\item \code{modelsummary} functions:
\itemize{
\item \code{fmt = fmt_significant(2)}: Two significant digits (at the term-level)
\item \code{fmt = fmt_sprintf("\%.3f")}: See \code{?sprintf}
\item \code{fmt = fmt_identity()}: unformatted raw values
}
}}

\item{leading_zero}{boolean. If \code{FALSE}, leading zeros are removed}

\item{diagonal}{character or NULL. If character, all elements of the
diagonal are replaced by the same character (e.g., "1").}

\item{upper_triangle}{character or NULL. If character, all elements of the
upper triangle are replaced by the same character (e.g., "" or ".").}

\item{stars}{to indicate statistical significance
\itemize{
\item FALSE (default): no significance stars.
\item TRUE: \code{c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)}
\item Named numeric vector for custom stars such as \code{c('*' = .1, '+' = .05)}
\item Note: a legend will not be inserted at the bottom of the table when the \code{estimate} or \code{statistic} arguments use "glue strings" with \code{{stars}}.
}}
}
\description{
Mostly for internal use, but can be useful when users supply a function to
the \code{method} argument of \code{datasummary_correlation}.
}
\examples{
library(modelsummary)

dat <- mtcars[, c("mpg", "hp", "disp")]

cor_fun <- function(x) {
  out <- cor(x, method = "kendall")
  datasummary_correlation_format(
    out,
    fmt = 2,
    upper_triangle = "x",
    diagonal = ".")
}

datasummary_correlation(dat, method = cor_fun)
}
