% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_gof.R
\name{get_gof}
\alias{get_gof}
\title{Extract model gof A mostly internal function with some potential uses
outside.}
\usage{
get_gof(model, ...)
}
\arguments{
\item{model}{a single model object}

\item{...}{all other arguments are passed through to the extractor and
table-making functions. This allows users to specify additional options such
as set `broom::tidy(exponentiate=TRUE)` to exponentiate logistic regression
coefficients or `kableExtra::kbl(escape=FALSE)` to avoid escaping math
characters in `kableExtra` tables.}
}
\description{
Extract model gof A mostly internal function with some potential uses
outside.
}
