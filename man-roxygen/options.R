#'
#' @section Global Options:
#'
#' The behavior of `modelsummary` can be modified by setting global options.  In particular, most of the arguments for most of the package's functions cna be set using global options. For example:
#'
#' * `options(modelsummary_output = "modelsummary_list")`
#' * `options(modelsummary_statistic = '({conf.low}, {conf.high})')`
#' * `options(modelsummary_stars = TRUE)`
#'
#' Options not specific to given arguments are listed below.
#'
#' ### Model labels: default column names
#'
#' These global option changes the style of the default column headers:
#'
#' * `options(modelsummary_model_labels = "roman")`
#'
#' The supported styles are: "model", "arabic", "letters", "roman", "(arabic)", "(letters)", "(roman)"
#'
#' ### Table-making packages
#'
#' `modelsummary` supports 6 table-making packages: `tinytable`, `kableExtra`, `gt`,
#' `flextable`, `huxtable`, and `DT`. Some of these packages have overlapping
#' functionalities. To change the default backend used for a specific file
#' format, you can use ' the `options` function:
#'
#' `options(modelsummary_factory_html = 'kableExtra')`
#' `options(modelsummary_factory_word = 'huxtable')`
#' `options(modelsummary_factory_png = 'gt')`
#' `options(modelsummary_factory_latex = 'gt')`
#' `options(modelsummary_factory_latex_tabular = 'kableExtra')`
#'
#' ### Table themes
#'
#' Change the look of tables in an automated and replicable way, using the `modelsummary` theming functionality. See the vignette: https://modelsummary.com/articles/appearance.html
#'
#' * `modelsummary_theme_gt`
#' * `modelsummary_theme_kableExtra`
#' * `modelsummary_theme_huxtable`
#' * `modelsummary_theme_flextable`
#' * `modelsummary_theme_dataframe`
#'
#' ### Model extraction functions
#'
#' `modelsummary` can use two sets of packages to extract information from
#' statistical models: the `easystats` family (`performance` and `parameters`)
#' and `broom`. By default, it uses `easystats` first and then falls back on
#' `broom` in case of failure. You can change the order of priorities or include
#' goodness-of-fit extracted by *both* packages by setting:
#'
#' `options(modelsummary_get = "easystats")`
#'
#' `options(modelsummary_get = "broom")`
#'
#' `options(modelsummary_get = "all")`
#'
#' ### Formatting numeric entries
#'
#' By default, LaTeX tables enclose all numeric entries in the `\num{}` command
#' from the siunitx package. To prevent this behavior, or to enclose numbers
#' in dollar signs (for LaTeX math mode), users can call:
#'
#' `options(modelsummary_format_numeric_latex = "plain")`
#'
#' `options(modelsummary_format_numeric_latex = "mathmode")`
#'
#' A similar option can be used to display numerical entries using MathJax in
#' HTML tables:
#'
#' `options(modelsummary_format_numeric_html = "mathjax")`
#'
#'
#' @section LaTeX preamble:
#'
#' When creating LaTeX via the `tinytable` backend (default in version 2.0.0 and later), it is useful to include the following commands in the LaTeX preamble of your documents. These commands are automatically added to the preamble when compiling Rmarkdown or Quarto documents, except when the `modelsummary()` calls are cached.
#'
#'
#' ```latex
#' \usepackage{tabularray}
#' \usepackage{float}
#' \usepackage{graphicx}
#' \usepackage[normalem]{ulem}
#' \UseTblrLibrary{booktabs}
#' \UseTblrLibrary{siunitx}
#' \newcommand{\tinytableTabularrayUnderline}[1]{\underline{#1}}
#' \newcommand{\tinytableTabularrayStrikeout}[1]{\sout{#1}}
#' \NewTableCommand{\tinytableDefineColor}[3]{\definecolor{#1}{#2}{#3}}
#' ```
#'
