#'
#' @section Global Options:
#'
#' The behavior of `modelsummary` can be affected by setting global options:
#'
#' + `modelsummary_factory_default`
#' + `modelsummary_factory_latex`
#' + `modelsummary_factory_html`
#' + `modelsummary_factory_png`
#' + `modelsummary_get`
#' + `modelsummary_format_numeric_latex`
#' + `modelsummary_format_numeric_html`
#' 
#' ### Table-making packages
#'
#' `modelsummary` supports 4 table-making packages: `kableExtra`, `gt`,
#' `flextable`, and `huxtable`. Some of these packages have overlapping
#' functionalities. For example, 3 of those packages can export to LaTeX. To
#' change the default backend used for a specific file format, you can use
#' the `options` function:
#'
#' `options(modelsummary_factory_html = 'kableExtra')`
#' `options(modelsummary_factory_latex = 'gt')`
#' `options(modelsummary_factory_word = 'huxtable')`
#' `options(modelsummary_factory_png = 'gt')`
#'
#' ### Model extraction functions
#' 
#' `modelsummary` can use two sets of packages to extract information from
#' statistical models: `broom` and the `easystats` family (`performance` and
#' `parameters`). By default, it uses `broom` first and `easystats` as a
#' fallback if `broom` fails. You can change the order of priorities
#' or include goodness-of-fit extracted by *both* packages by setting:
#'
#' `options(modelsummary_get = "broom")`
#' `options(modelsummary_get = "easystats")`
#' `options(modelsummary_get = "all")`
#'
#' ### Formatting numeric entries
#' 
#' By default, LaTeX tables enclose all numeric entries in the `\num{}` command
#' from the siunitx package. To prevent this behavior, or to enclose numbers
#' in dollar signs (for LaTeX math mode), users can call:
#'
#' `options(modelsummary_format_numeric_latex = "plain")`
#' `options(modelsummary_format_numeric_latex = "mathmode")`
#'
#' A similar option can be used to display numerical entries using MathJax in
#' HTML tables:
#'
#' `options(modelsummary_format_numeric_html = "mathjax")`
