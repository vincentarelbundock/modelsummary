% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config_modelsummary.R
\name{config_modelsummary}
\alias{config_modelsummary}
\title{Persistent user settings for the \code{modelsummary} package}
\usage{
config_modelsummary(
  factory_default,
  factory_latex,
  factory_html,
  factory_markdown,
  startup_message,
  reset = FALSE
)
}
\arguments{
\item{factory_default}{Default output format: "tinytable", "kableExtra", "gt", "flextable", "huxtable", "DT", or "markdown"}

\item{factory_latex}{Name of package used to generate LaTeX output when \code{output="latex"}.}

\item{factory_html}{Name of package used to generate LaTeX output when \code{output="html"}.}

\item{factory_markdown}{Name of package used to generate LaTeX output when \code{output="markdown"}.}

\item{startup_message}{TRUE or FALSE to show warnings at startup}

\item{reset}{TRUE to return to default settings.}
}
\description{
Persistent user settings for the \code{modelsummary} package
}
