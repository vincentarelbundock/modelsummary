% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasummary_correlation.R
\name{datasummary_correlation}
\alias{datasummary_correlation}
\title{Generate a correlation table for all numeric variables in your dataset.}
\usage{
datasummary_correlation(
  data,
  output = "default",
  fmt = 2,
  title = NULL,
  notes = NULL,
  method = "pearson",
  ...
)
}
\arguments{
\item{data}{A data.frame (or tibble)}

\item{output}{filename or object type (character string)
\itemize{
  \item Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg.
  \item Supported object types: "default", "html", "markdown", "latex", "latex_tabular", "data.frame", "gt", "kableExtra", "huxtable", "flextable".
  \item Warning: the `output` argument \emph{cannot} be used when customizing tables with external packages. See the 'Details' section below.
}}

\item{fmt}{determines how to format numeric values
\itemize{
  \item integer: the number of digits to keep after the period `format(round(x, fmt), nsmall=fmt)`
  \item character: passed to the `sprintf` function (e.g., '\%.3f' keeps 3 digits with trailing zero). See `?sprintf`
  \item function: returns a formatted character string.
}}

\item{title}{string}

\item{notes}{list or vector of notes to append to the bottom of the table.}

\item{method}{character or function
\itemize{
  \item character: "pearson", "kendall", "spearman", or "pearspear"
    (Pearson correlations above and Spearman correlations below the diagonal)
  \item function: takes a data.frame with numeric columns and returns a
    square matrix or data.frame with unique row.names and colnames
    corresponding to variable names. Note that the
    `datasummary_correlation_format` can often be useful for formatting the
    output of custom correlation functions.
}}
}
\description{
The names of the variables displayed in the correlation table are the names
of the columns in the `data`. You can rename those columns (with or without
spaces) to produce a table of human-readable variables.
}
\examples{
library(modelsummary)

# clean variable names (base R)
dat <- mtcars[, c("mpg", "hp")]
colnames(dat) <- c("Miles / Gallon", "Horse Power")
datasummary_correlation(dat)

# clean variable names (tidyverse)
library(tidyverse)
dat <- mtcars \%>\% 
  select(`Miles / Gallon` = mpg,
         `Horse Power` = hp)
datasummary_correlation(dat)

# alternative methods
datasummary_correlation(dat, method = "pearspear")

# custom function
cor_fun <- function(x) cor(x, method = "kendall")
datasummary_correlation(dat, method = cor_fun)

# rename columns alphabetically and include a footnote for reference
note <- sprintf("(\%s) \%s", letters[1:ncol(dat)], colnames(dat))
note <- paste(note, collapse = "; ")

colnames(dat) <- sprintf("(\%s)", letters[1:ncol(dat)])

datasummary_correlation(dat, notes = note)

# `datasummary_correlation_format`: custom function with formatting
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

# use kableExtra and psych to color significant cells
library(psych)
library(kableExtra)

dat <- mtcars[, c("vs", "hp", "gear")]

cor_fun <- function(dat) {
  # compute correlations and format them
  correlations <- data.frame(cor(dat))
  correlations <- datasummary_correlation_format(correlations, fmt = 2)

  # calculate pvalues using the `psych` package
  pvalues <- psych::corr.test(dat)$p

  # use `kableExtra::cell_spec` to color significant cells
  for (i in 1:nrow(correlations)) {
    for (j in 1:ncol(correlations)) {
      if (pvalues[i, j] < 0.05 && i != j) {
        correlations[i, j] <- cell_spec(correlations[i, j], background = "pink")
      }
    }
  }
  return(correlations)
}

# The `escape=FALSE` is important here!
datasummary_correlation(dat, method = cor_fun, escape = FALSE)
}
