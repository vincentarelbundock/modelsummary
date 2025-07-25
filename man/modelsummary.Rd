% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelsummary.R
\name{modelsummary}
\alias{modelsummary}
\title{Model Summary Tables}
\usage{
modelsummary(
  models,
  output = getOption("modelsummary_output", default = "default"),
  fmt = getOption("modelsummary_fmt", default = 3),
  estimate = getOption("modelsummary_estimate", default = "estimate"),
  statistic = getOption("modelsummary_statistic", default = "std.error"),
  vcov = getOption("modelsummary_vcov", default = NULL),
  conf_level = getOption("modelsummary_conf_level", default = 0.95),
  exponentiate = getOption("modelsummary_exponentiate", default = FALSE),
  stars = getOption("modelsummary_stars", default = FALSE),
  shape = getOption("modelsummary_shape", default = term + statistic ~ model),
  coef_map = getOption("modelsummary_coef_map", default = NULL),
  coef_omit = getOption("modelsummary_coef_omit", default = NULL),
  coef_rename = getOption("modelsummary_coef_rename", default = FALSE),
  gof_map = getOption("modelsummary_gof_map", default = NULL),
  gof_omit = getOption("modelsummary_gof_omit", default = NULL),
  gof_function = getOption("modelsummary_gof_function", default = NULL),
  group_map = getOption("modelsummary_group_map", default = NULL),
  add_columns = getOption("modelsummary_add_columns", default = NULL),
  add_rows = getOption("modelsummary_add_rows", default = NULL),
  align = getOption("modelsummary_align", default = NULL),
  notes = getOption("modelsummary_notes", default = NULL),
  title = getOption("modelsummary_title", default = NULL),
  escape = getOption("modelsummary_escape", default = TRUE),
  ...
)
}
\arguments{
\item{models}{a model, (named) list of models, or nested list of models.
\itemize{
\item Single model: \code{modelsummary(model)}
\item Unnamed list of models: \code{modelsummary(list(model1, model2))}
\itemize{
\item Models are labelled automatically. The default label style can be altered by setting a global option. See below.
}
\item Named list of models: \code{modelsummary(list("A"=model1, "B"=model2))}
\itemize{
\item Models are labelled using the list names.
}
\item Nested list of models:
\itemize{
\item When using the \code{shape} argument with "rbind", "rcollapse", or "cbind" values, \code{models} can be a nested list of models to display "panels" or "stacks" of regression models. See the \code{shape} argument documentation and examples below.
}
}}

\item{output}{filename or object type (character string)
\itemize{
\item Supported filename extensions: .docx, .html, .tex, .md, .txt, .csv, .xlsx, .png, .jpg
\item Supported object types: "default", "html", "markdown", "latex", "latex_tabular", "typst", "data.frame", "tinytable", "gt", "kableExtra", "huxtable", "flextable", "DT", "jupyter". The "modelsummary_list" value produces a lightweight object which can be saved and fed back to the \code{modelsummary} function.
\item The "default" output format can be set to "tinytable", "kableExtra", "gt", "flextable", "huxtable", "DT", or "markdown"
\itemize{
\item If the user does not choose a default value, the packages listed above are tried in sequence.
\item Session-specific configuration: \code{options("modelsummary_factory_default" = "gt")}
\item Persistent configuration: \code{config_modelsummary(output = "markdown")}
}
\item Warning: Users should not supply a file name to the \code{output} argument if they intend to customize the table with external packages. See the 'Details' section.
\item LaTeX compilation requires the \code{booktabs} and \code{siunitx} packages, but \code{siunitx} can be disabled or replaced with global options. See the 'Details' section.
}}

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
\item \code{fmt = fmt_decimal(digits = 2, pdigits = 3)}: Decimal digits for estimate and p values
\item \code{fmt = fmt_sprintf("\%.3f")}: See \code{?sprintf}
\item \code{fmt = fmt_term("(Intercept)" = 1, "X" = 2)}: Format terms differently
\item \code{fmt = fmt_statistic("estimate" = 1, "r.squared" = 6)}: Format statistics differently.
\item \code{fmt = fmt_identity()}: unformatted raw values
}
\item string:
\item Note on LaTeX output: To ensure proper typography, all numeric entries are enclosed in the \verb{\\num\{\}} command, which requires the \code{siunitx} package to be loaded in the LaTeX preamble. This behavior can be altered with global options. See the 'Details' section.
}}

\item{estimate}{a single string or a character vector of length equal to the
number of models. Valid entries include any column name of
the data.frame produced by \code{get_estimates(model)}, and strings with curly braces compatible with the \code{glue} package format. Examples:
\itemize{
\item \code{"estimate"}
\item \code{"{estimate} ({std.error}){stars}"}
\item \code{"{estimate} [{conf.low}, {conf.high}]"}
}}

\item{statistic}{vector of strings or \code{glue} strings which select uncertainty statistics to report vertically below the estimate. NULL omits all uncertainty statistics.
\itemize{
\item "conf.int", "std.error", "statistic", "p.value", "conf.low", "conf.high", or any column name produced by \code{get_estimates(model)}
\item \code{glue} package strings with braces, with or without R functions, such as:
\itemize{
\item \code{"{p.value} [{conf.low}, {conf.high}]"}
\item \code{"Std.Error: {std.error}"}
\item \code{"{exp(estimate) * std.error}"}
\item Numbers are automatically rounded and converted to strings. To apply functions to their numeric values, as in the last \code{glue} example, users must set \code{fmt=NULL}.
\item Parentheses are added automatically unless the string includes \code{glue} curly braces \code{{}}.
}
\item Notes:
\itemize{
\item The names of the \code{statistic} are used a column names when using the \code{shape} argument to display statistics as columns:
\itemize{
\item \code{statistic=c("p"="p.value", "["="conf.low", "]"="conf.high")}
}
\item Some statistics are not supported for all models. See column names in \code{get_estimates(model)}, and visit the website to learn how to add custom statistics.
}
}}

\item{vcov}{robust standard errors and other manual statistics. The \code{vcov}
argument accepts six types of input (see the 'Details' and 'Examples'
sections below):
\itemize{
\item NULL returns the default uncertainty estimates of the model object
\item string, vector, or (named) list of strings. "iid", "classical", and "constant" are aliases for \code{NULL}, which returns the model's default uncertainty estimates. The strings "HC", "HC0", "HC1" (alias: "stata"), "HC2", "HC3" (alias: "robust"), "HC4", "HC4m", "HC5", "HAC", "NeweyWest", "Andrews", "panel-corrected", "outer-product", and "weave" use variance-covariance matrices computed using functions from the \code{sandwich} package, or equivalent method. "BS", "bootstrap", "residual", "mammen", "webb", "xy", "wild" use the \code{sandwich::vcovBS()}. The behavior of those functions can (and sometimes \emph{must}) be altered by passing arguments to \code{sandwich} directly from \code{modelsummary} through the ellipsis (\code{...}), but it is safer to define your own custom functions as described in the next bullet.
\item function or (named) list of functions which return variance-covariance matrices with row and column names equal to the names of your coefficient estimates (e.g., \code{stats::vcov}, \code{sandwich::vcovHC}, \code{function(x) vcovPC(x, cluster="country")}).
\item formula or (named) list of formulas with the cluster variable(s) on the right-hand side (e.g., ~clusterid).
\item named list of \code{length(models)} variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
\item a named list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. Warning: since this list of vectors can include arbitrary strings or numbers, \code{modelsummary} cannot automatically calculate p values. The \code{stars} argument may thus use incorrect significance thresholds when \code{vcov} is a list of vectors.
}}

\item{conf_level}{numeric value between 0 and 1. confidence level to use for
confidence intervals. Setting this argument to \code{NULL} does not extract
confidence intervals, which can be faster for some models.}

\item{exponentiate}{TRUE, FALSE, or logical vector of length equal to the
number of models. If TRUE, the \code{estimate}, \code{conf.low}, and \code{conf.high}
statistics are exponentiated, and the \code{std.error} is transformed to
\code{exp(estimate)*std.error}. The \code{exponentiate} argument is ignored for
distributional random effects parameters (SD and Cor).}

\item{stars}{to indicate statistical significance
\itemize{
\item FALSE (default): no significance stars.
\item TRUE: \code{c("+" = .1, "*" = .05, "**" = .01, "***" = 0.001)}
\item Named numeric vector for custom stars such as \code{c('*' = .1, '+' = .05)}
\item Note: a legend will not be inserted at the bottom of the table when the \code{estimate} or \code{statistic} arguments use "glue strings" with \code{{stars}}.
}}

\item{shape}{\code{NULL}, formula, or string which determines the shape of a table.
\itemize{
\item \code{NULL}: Default shape with terms in rows and models in columns.
\item Formula: The left side determines what appears on rows, and the right side determines what appears on columns. The formula can include one or more group identifier(s) to display related terms together, which can be useful for models with multivariate outcomes or grouped coefficients (See examples section below). The group identifier(s) must be column names produced by: \code{get_estimates(model)}. The group identifier(s) can be combined with the term identifier in a single column by using the colon to represent an interaction. If an incomplete formula is supplied (e.g., \code{~statistic}), \code{modelsummary} tries to complete it automatically. Goodness-of-fit statistics are only appended to the bottom of the table when \code{model} is on the right hand side of the formula (i.e., columns). Potential \code{shape} values include:
\itemize{
\item \code{term + statistic ~ model}: default
\item \code{term ~ model + statistic}: statistics in separate columns
\item \code{model + statistic ~ term}: models in rows and terms in columns
\item \code{term + response + statistic ~ model}: term and group id in separate columns
\item \code{term : response + statistic ~ model}: term and group id in a single column
\item \code{term ~ response}
}
\item String: "cbind", "rbind", "rcollapse"
\itemize{
\item "cbind": side-by-side models with autmoatic spanning column headers to group models (\code{tinytable} only feature).
\item "rbind" or "rcollapse": "panels" or "stacks" of regression models.
\item the \code{models} argument must be a (potentially named) nested list of models.
}
\itemize{
\item Unnamed nested list with 2 panels: \code{list(list(model1, model2), list(model3, model4))}
\item Named nested list with 2 panels: \code{list("Panel A" = list(model1, model2), "Panel B" = list(model3, model4))}
\item Named panels and named models: \code{list("Panel A" = list("(I)" = model1, "(II)" = model2), "Panel B" = list("(I)" = model3, "(II)" = model4))}
}
\itemize{
\item "rbind": Bind the rows of independent regression tables
\item "rcollapse": Bind the rows of regression tables and create a panel at the bottom where we "collapse" goodness-of-fit statistics which are identical across models.
}
}}

\item{coef_map}{character vector. Subset, rename, and reorder coefficients.
Coefficients omitted from this vector are omitted from the table. The order
of the vector determines the order of the table. \code{coef_map} can be a named
or an unnamed character vector. If \code{coef_map} is a named vector, its values
define the labels that must appear in the table, and its names identify the
original term names stored in the model object: \code{c("hp:mpg"="HPxM/G")}. See
Examples section below.}

\item{coef_omit}{integer vector or regular expression to identify which coefficients to omit (or keep) from the table. Positive integers determine which coefficients to omit. Negative integers determine which coefficients to keep. A regular expression can be used to omit coefficients, and perl-compatible "negative lookaheads" can be used to specify which coefficients to \emph{keep} in the table. Examples:
\itemize{
\item c(2, 3, 5): omits the second, third, and fifth coefficients.
\item c(-2, -3, -5): negative values keep the second, third, and fifth coefficients.
\item \code{"ei"}: omit coefficients matching the "ei" substring.
\item \code{"^Volume$"}: omit the "Volume" coefficient.
\item \code{"ei|rc"}: omit coefficients matching either the "ei" or the "rc" substrings.
\item \code{"^(?!Vol)"}: keep coefficients starting with "Vol" (inverse match using a negative lookahead).
\item \code{"^(?!.*ei)"}: keep coefficients matching the "ei" substring.
\item \code{"^(?!.*ei|.*pt)"}: keep coefficients matching either the "ei" or the "pt" substrings.
\item See the Examples section below for complete code.
}}

\item{coef_rename}{logical, named or unnamed character vector, or function
\itemize{
\item Logical: TRUE renames variables based on the "label" attribute of each column. See the Example section below. Note: renaming is done by the \code{parameters} package at the extraction stage, before other arguments are applied like \code{coef_omit}.
\item Unnamed character vector of length equal to the number of coefficients in the final table, after \code{coef_omit} is applied.
\item Named character vector: Values refer to the variable names that will appear in the table. Names refer to the original term names stored in the model object. Ex: c("hp:mpg"="hp X mpg")
\item Function: Accepts a character vector of the model's term names and returns a named vector like the one described above. The \code{modelsummary} package supplies a \code{coef_rename()} function which can do common cleaning tasks: \code{modelsummary(model, coef_rename = coef_rename)}
}}

\item{gof_map}{rename, reorder, and omit goodness-of-fit statistics and other
model information. This argument accepts 4 types of values:
\itemize{
\item NULL (default): the \code{modelsummary::gof_map} dictionary is used for formatting, and all unknown statistic are included.
\item character vector: "all", "none", or a vector of statistics such as \code{c("rmse", "nobs", "r.squared")}. Elements correspond to colnames in the data.frame produced by \code{get_gof(model)}. The \code{modelsummary::gof_map} default dictionary is used to format and rename statistics.
\item NA: excludes all statistics from the bottom part of the table.
\item data.frame with 3 columns named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples' section below. The \code{fmt} column in this data frame only accepts integers. For more flexibility, use a list of lists, as described in the next bullet.
\item list of lists, each of which includes 3 elements named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples section below'.
}}

\item{gof_omit}{string regular expression (perl-compatible) used to determine which statistics to omit from the bottom section of the table. A "negative lookahead" can be used to specify which statistics to \emph{keep} in the table. Examples:
\itemize{
\item \code{"IC"}: omit statistics matching the "IC" substring.
\item \code{"BIC|AIC"}: omit statistics matching the "AIC" or "BIC" substrings.
\item \code{"^(?!.*IC)"}: keep statistics matching the "IC" substring.
}}

\item{gof_function}{function which accepts a model object in the \code{model} argument and returns a 1-row \code{data.frame} with one custom goodness-of-fit statistic per column.}

\item{group_map}{named or unnamed character vector. Subset, rename, and
reorder coefficient groups specified a grouping variable specified in the
\code{shape} argument formula. This argument behaves like \code{coef_map}.}

\item{add_columns}{a data.frame (or tibble) with the same number of rows as
#' your main table. By default, rows are appended to the bottom of the table.
You can define a "position" attribute of integers to set the columns positions.
See Examples section below.}

\item{add_rows}{a data.frame (or tibble) with the same number of columns as your main table. By default, rows are appended to the bottom of the table. Positions can be defined using integers. In the \code{modelsummary()} function (only), you can also use string shortcuts: "coef_start", "coef_end", "gof_start", "gof_end"
\itemize{
\item \code{attr(new_rows, 1:2)}
\item \code{attr(new_rows, "gof_start")}
See Examples section below.
}}

\item{align}{A string with a number of characters equal to the number of columns in
the table (e.g., \code{align = "lcc"}).  Valid characters: l, c, r, d.
\itemize{
\item "l": left-aligned column
\item "c": centered column
\item "r": right-aligned column
\item "d": dot-aligned column. For LaTeX/PDF output, this option requires at least version 3.0.25 of the siunitx LaTeX package. See the LaTeX preamble help section below for commands to insert in your LaTeX preamble.
}}

\item{notes}{list or vector of notes to append to the bottom of the table.}

\item{title}{string. Cross-reference labels should be added with Quarto or Rmarkdown chunk options when applicable. When saving standalone LaTeX files, users can add a label such as \verb{\\\\label\{tab:mytable\}} directly to the title string, while also specifying \code{escape=FALSE}.}

\item{escape}{boolean TRUE escapes or substitutes LaTeX/HTML characters which could
prevent the file from compiling/displaying. \code{TRUE} escapes all cells, captions, and notes. Users can have more fine-grained control by setting \code{escape=FALSE} and using an external command such as: \code{modelsummary(model, "latex") |> tinytable::format_tt(tab, j=1:5, escape=TRUE)}}

\item{...}{all other arguments are passed through to three functions. See the documentation of these functions for lists of available arguments.
\itemize{
\item \link[parameters:model_parameters]{parameters::model_parameters} extracts parameter estimates. Available arguments depend on model type, but include:
\itemize{
\item \code{standardize}, \code{include_reference}, \code{centrality}, \code{dispersion}, \code{test}, \code{ci_method}, \code{prior}, \code{diagnostic}, \code{rope_range}, \code{power}, \code{cluster}, etc.
}
\item \link[performance:model_performance]{performance::model_performance} extracts goodness-of-fit statistics. Available arguments depend on model type, but include:
\itemize{
\item \code{metrics}, \code{estimator}, etc.
}
\item \link[tinytable:tt]{tinytable::tt}, \link[kableExtra:kbl]{kableExtra::kbl} or \link[gt:gt]{gt::gt} draw tables, depending on the value of the \code{output} argument. For example, by default \code{modelsummary} creates tables with \link[tinytable:tt]{tinytable::tt}, which accepts a \code{width} and \code{theme} arguments.
}}
}
\value{
a regression table in a format determined by the \code{output} argument.
}
\description{
Create beautiful and customizable tables to summarize several statistical
models side-by-side. This function supports dozens of statistical models,
and it can produce tables in HTML, LaTeX, Word, Markdown, Typst, PDF, PowerPoint,
Excel, RTF, JPG, or PNG. The appearance of the tables can be customized
extensively by specifying the \code{output} argument, and by using functions from
one of the supported table customization packages: \code{tinytable}, \code{kableExtra}, \code{gt},
\code{flextable}, \code{huxtable}, \code{DT}. For more information, see the Details and Examples
sections below, and the vignettes on the \code{modelsummary} website:
https://modelsummary.com/
\itemize{
\item \href{https://modelsummary.com/vignettes/modelsummary.html}{The \code{modelsummary} Vignette includes dozens of examples of tables with extensive customizations.}
\item \href{https://modelsummary.com/vignettes/appearance.html}{The Appearance Vignette shows how to modify the look of tables.}
}
}
\details{
\subsection{\code{output}}{

The \code{modelsummary_list} output is a lightweight format which can be used to save model results, so they can be fed back to \code{modelsummary} later to avoid extracting results again.

When a file name with a valid extension is supplied to the \code{output} argument,
the table is written immediately to file. If you want to customize your table
by post-processing it with an external package, you need to choose a
different output format and saving mechanism. Unfortunately, the approach
differs from package to package:
\itemize{
\item \code{tinytable}: set \code{output="tinytable"}, post-process your table, and use the \code{tinytable::save_tt} function.
\item \code{gt}: set \code{output="gt"}, post-process your table, and use the \code{gt::gtsave} function.
\item \code{kableExtra}: set \code{output} to your destination format (e.g., "latex", "html", "markdown"), post-process your table, and use \code{kableExtra::save_kable} function.
}
}

\subsection{\code{vcov}}{

To use a string such as "robust" or "HC0", your model must be supported
by the \code{sandwich} package. This includes objects such as: lm, glm,
survreg, coxph, mlogit, polr, hurdle, zeroinfl, and more.

NULL, "classical", "iid", and "constant" are aliases which do not modify
uncertainty estimates and simply report the default standard errors stored
in the model object.

One-sided formulas such as \code{~clusterid} are passed to the \code{sandwich::vcovCL}
function.

Matrices and functions producing variance-covariance matrices are first
passed to \code{lmtest}. If this does not work, \code{modelsummary} attempts to take
the square root of the diagonal to adjust "std.error", but the other
uncertainty estimates are not be adjusted.

Numeric vectors are formatted according to \code{fmt} and placed in brackets.
Character vectors printed as given, without parentheses.

If your model type is supported by the \code{lmtest} package, the
\code{vcov} argument will try to use that package to adjust all the
uncertainty estimates, including "std.error", "statistic", "p.value", and
"conf.int". If your model is not supported by \code{lmtest}, only the "std.error"
will be adjusted by, for example, taking the square root of the matrix's
diagonal.
}
}
\section{Version 2.0.0, kableExtra, and tinytable}{


Since version 2.0.0, \code{modelsummary} uses \code{tinytable} as its default table-drawing backend.
Learn more at: https://vincentarelbundock.github.io/tinytable/",

Revert to \code{kableExtra} for one session:

\code{options(modelsummary_factory_default = 'kableExtra')}
\code{options(modelsummary_factory_latex = 'kableExtra')}
\code{options(modelsummary_factory_html = 'kableExtra')}
}

\section{Global Options}{


The behavior of \code{modelsummary} can be modified by setting global options.  In particular, most of the arguments for most of the package's functions cna be set using global options. For example:
\itemize{
\item \code{options(modelsummary_output = "modelsummary_list")}
\item \code{options(modelsummary_statistic = '({conf.low}, {conf.high})')}
\item \code{options(modelsummary_stars = TRUE)}
}

Options not specific to given arguments are listed below.
\subsection{Model labels: default column names}{

These global option changes the style of the default column headers:
\itemize{
\item \code{options(modelsummary_model_labels = "roman")}
}

The supported styles are: "model", "arabic", "letters", "roman", "(arabic)", "(letters)", "(roman)"
}

\subsection{Table-making packages}{

\code{modelsummary} supports 6 table-making packages: \code{tinytable}, \code{kableExtra}, \code{gt},
\code{flextable}, \code{huxtable}, and \code{DT}. Some of these packages have overlapping
functionalities. To change the default backend used for a specific file
format, you can use ' the \code{options} function:

\code{options(modelsummary_factory_html = 'kableExtra')}
\code{options(modelsummary_factory_word = 'huxtable')}
\code{options(modelsummary_factory_png = 'gt')}
\code{options(modelsummary_factory_latex = 'gt')}
\code{options(modelsummary_factory_latex_tabular = 'kableExtra')}
}

\subsection{Table themes}{

Change the look of tables in an automated and replicable way, using the \code{modelsummary} theming functionality. See the vignette: https://modelsummary.com/vignettes/appearance.html
\itemize{
\item \code{modelsummary_theme_gt}
\item \code{modelsummary_theme_kableExtra}
\item \code{modelsummary_theme_huxtable}
\item \code{modelsummary_theme_flextable}
\item \code{modelsummary_theme_dataframe}
}
}

\subsection{Model extraction functions}{

\code{modelsummary} can use two sets of packages to extract information from
statistical models: the \code{easystats} family (\code{performance} and \code{parameters})
and \code{broom}. By default, it uses \code{easystats} first and then falls back on
\code{broom} in case of failure. You can change the order of priorities or include
goodness-of-fit extracted by \emph{both} packages by setting:

\code{options(modelsummary_get = "easystats")}

\code{options(modelsummary_get = "broom")}

\code{options(modelsummary_get = "all")}
}

\subsection{Formatting numeric entries}{

By default, LaTeX tables enclose all numeric entries in the \verb{\\num\{\}} command
from the siunitx package. To prevent this behavior, or to enclose numbers
in dollar signs (for LaTeX math mode), users can call:

\code{options(modelsummary_format_numeric_latex = "plain")}

\code{options(modelsummary_format_numeric_latex = "mathmode")}

A similar option can be used to display numerical entries using MathJax in
HTML tables:

\code{options(modelsummary_format_numeric_html = "mathjax")}
}
}

\section{LaTeX preamble}{


When creating LaTeX via the \code{tinytable} backend (default in version 2.0.0 and later), it is useful to include the following commands in the LaTeX preamble of your documents. These commands are automatically added to the preamble when compiling Rmarkdown or Quarto documents, except when the \code{modelsummary()} calls are cached.

\if{html}{\out{<div class="sourceCode latex">}}\preformatted{\\usepackage\{tabularray\}
\\usepackage\{float\}
\\usepackage\{graphicx\}
\\usepackage[normalem]\{ulem\}
\\UseTblrLibrary\{booktabs\}
\\UseTblrLibrary\{siunitx\}
\newcommand{\tinytableTabularrayUnderline}[1]\{\\underline\{#1\}\}
\newcommand{\tinytableTabularrayStrikeout}[1]\{\\sout\{#1\}\}
\\NewTableCommand\{\\tinytableDefineColor\}[3]\{\\definecolor\{#1\}\{#2\}\{#3\}\}
}\if{html}{\out{</div>}}
}

\section{Parallel computation}{


It can take a long time to compute and extract summary statistics from
certain models (e.g., Bayesian). In those cases, users can parallelize the
process. Since parallelization occurs at the model level, no speedup is
available for tables with a single model. Users on mac or linux can launch
parallel computation using the built-in \code{parallel} package. All they need to
do is supply a \code{mc.cores} argument which will be pushed forward to the
\code{parallel::mclapply} function:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{modelsummary(model_list, mc.cores = 5)
}\if{html}{\out{</div>}}

All users can also use the \code{future.apply} package to parallelize model summaries.
For example, to use 4 cores to extract results:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{library(future.apply)
plan(multicore, workers = 4)
options("modelsummary_future" = TRUE)
modelsummary(model_list)
}\if{html}{\out{</div>}}

Note that the "multicore" plan only parallelizes under mac or linux. Windows
users can use \code{plan(multisession)} instead. However, note that the first
time \code{modelsummary()} is called under multisession can be a fair bit longer,
because of extra costs in passing data to and loading required packages on
to workers. Subsequent calls to \code{modelsummary()} will often be much faster.

Some users have reported difficult to reproduce errors when using the
\code{future} package with some packages. The \code{future} parallelization in
\code{modelsummary} can be disabled by calling:

\code{options("modelsummary_future" = FALSE)}
}

\examples{
\dontshow{if (isTRUE(Sys.getenv("R_NOT_CRAN") == "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# The `modelsummary` website includes \emph{many} examples and tutorials:
# https://modelsummary.com

library(modelsummary)

# load data and estimate models
utils::data(trees)
models <- list()
models[["Bivariate"]] <- lm(Girth ~ Height, data = trees)
models[["Multivariate"]] <- lm(Girth ~ Height + Volume, data = trees)

# simple table
modelsummary(models)

# statistic
modelsummary(models, statistic = NULL)

modelsummary(models, statistic = "p.value")

modelsummary(models, statistic = "statistic")

modelsummary(models, statistic = "conf.int", conf_level = 0.99)

modelsummary(models, statistic = c(
  "t = {statistic}",
  "se = {std.error}",
  "conf.int"))

# estimate
modelsummary(models,
  statistic = NULL,
  estimate = "{estimate} [{conf.low}, {conf.high}]")

modelsummary(models,
  estimate = c(
    "{estimate}{stars}",
    "{estimate} ({std.error})"))

# vcov
modelsummary(models, vcov = "robust")

modelsummary(models, vcov = list("classical", "stata"))

modelsummary(models, vcov = sandwich::vcovHC)

modelsummary(models,
  vcov = list(stats::vcov, sandwich::vcovHC))

modelsummary(models,
  vcov = list(
    c("(Intercept)" = "", "Height" = "!"),
    c("(Intercept)" = "", "Height" = "!", "Volume" = "!!")))

# vcov with custom names
modelsummary(
  models,
  vcov = list(
    "Stata Corp" = "stata",
    "Newey Lewis & the News" = "NeweyWest"))

# fmt
mod <- lm(mpg ~ hp + drat + qsec, data = mtcars)

modelsummary(mod, fmt = 3)

modelsummary(mod, fmt = fmt_significant(3))

modelsummary(mod, fmt = NULL)

modelsummary(mod, fmt = fmt_decimal(4))

modelsummary(mod, fmt = fmt_sprintf("\%.5f"))

modelsummary(mod, fmt = fmt_statistic(estimate = 4, conf.int = 1), statistic = "conf.int")

modelsummary(mod, fmt = fmt_term(hp = 4, drat = 1, default = 2))

m <- lm(mpg ~ I(hp * 1000) + drat, data = mtcars)
f <- function(x) format(x, digits = 3, nsmall = 2, scientific = FALSE, trim = TRUE)
modelsummary(m, fmt = f, gof_map = NA)

# coef_rename
modelsummary(models, coef_rename = c("Volume" = "Large", "Height" = "Tall"))

modelsummary(models, coef_rename = toupper)

modelsummary(models, coef_rename = coef_rename)

# coef_rename = TRUE for variable labels
datlab <- mtcars
datlab$cyl <- factor(datlab$cyl)
attr(datlab$hp, "label") <- "Horsepower"
attr(datlab$cyl, "label") <- "Cylinders"
modlab <- lm(mpg ~ hp * drat + cyl, data = datlab)
modelsummary(modlab, coef_rename = TRUE)

# coef_rename: unnamed vector of length equal to the number of terms in the final table
m <- lm(hp ~ mpg + factor(cyl), data = mtcars)
modelsummary(m, coef_omit = -(3:4), coef_rename = c("Cyl 6", "Cyl 8"))

# coef_map
modelsummary(models, coef_map = c("Volume" = "Large", "Height" = "Tall"))

modelsummary(models, coef_map = c("Volume", "Height"))

# coef_omit: omit the first and second coefficients
modelsummary(models, coef_omit = 1:2)

# coef_omit: omit coefficients matching one substring
modelsummary(models, coef_omit = "ei", gof_omit = ".*")

# coef_omit: omit a specific coefficient
modelsummary(models, coef_omit = "^Volume$", gof_omit = ".*")

# coef_omit: omit coefficients matching either one of two substring
# modelsummary(models, coef_omit = "ei|rc", gof_omit = ".*")

# coef_omit: keep coefficients starting with a substring (using a negative lookahead)
# modelsummary(models, coef_omit = "^(?!Vol)", gof_omit = ".*")

# coef_omit: keep coefficients matching a substring
modelsummary(models, coef_omit = "^(?!.*ei|.*pt)", gof_omit = ".*")

# shape: multinomial model
library(nnet)
multi <- multinom(factor(cyl) ~ mpg + hp, data = mtcars, trace = FALSE)

# shape: term names and group ids in rows, models in columns
modelsummary(multi, shape = response ~ model)

# shape: term names and group ids in rows in a single column
modelsummary(multi, shape = term:response ~ model)

# shape: term names in rows and group ids in columns
modelsummary(multi, shape = term ~ response:model)

# shape = "rcollapse"
panels <- list(
  "Panel A: MPG" = list(
    "A" = lm(mpg ~ hp, data = mtcars),
    "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
  "Panel B: Displacement" = list(
    "A" = lm(disp ~ hp, data = mtcars),
    "C" = lm(disp ~ hp + factor(gear), data = mtcars))
)

# shape = "cbind"
modelsummary(panels, shape = "cbind")

modelsummary(
  panels,
  shape = "rbind",
  gof_map = c("nobs", "r.squared"))

# title
modelsummary(models, title = "This is the title")

# title with LaTeX label (for numbering and referencing)
modelsummary(models, title = "This is the title \\\\label{tab:description}", escape = FALSE)

# add_rows
rows <- tibble::tribble(
  ~term, ~Bivariate, ~Multivariate,
  "Empty row", "-", "-",
  "Another empty row", "?", "?")
attr(rows, "position") <- c(1, 3)
modelsummary(models, add_rows = rows)

attr(rows, "position") <- "gof_start"
modelsummary(models, add_rows = rows)

# notes
modelsummary(models, notes = list("A first note", "A second note"))

# gof_map: tribble
library(tibble)
gm <- tribble(
  ~raw, ~clean, ~fmt,
  "r.squared", "R Squared", 5)
modelsummary(models, gof_map = gm)

# gof_map: list of lists
f <- function(x) format(round(x, 3), big.mark = ",")
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f),
  list("raw" = "AIC", "clean" = "aic", "fmt" = f))
modelsummary(models, gof_map = gm)
\dontshow{\}) # examplesIf}
}
\references{
Arel-Bundock V (2022). “modelsummary: Data and Model Summaries in R.” \emph{Journal of Statistical Software}, \emph{103}(1), 1-23. \doi{10.18637/jss.v103.i01}.'
}
