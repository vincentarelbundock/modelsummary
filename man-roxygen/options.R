#'
#' @section Global Options:
#'
#' The behavior of `modelsummary` can be modified by setting global options. For example:
#' 
#' * `options(modelsummary_model_labels = "roman")`
#' 
#' The rest of this section describes each of the options above.
#' 
#' ### Model labels: default column names
#' 
#' These global option changes the style of the default column headers:
#' 
#' * `options(modelsummary_model_labels = "roman")`
#' * `options(modelsummary_panel_labels = "roman")`
#' 
#' The supported styles are: "model", "panel", "arabic", "letters", "roman", "(arabic)", "(letters)", "(roman)""
#' 
#' The panel-specific option is only used when `shape="rbind"`
#' 
#' ### Table-making packages
#'
#' `modelsummary` supports 4 table-making packages: `kableExtra`, `gt`,
#' `flextable`, `huxtable`, and `DT`. Some of these packages have overlapping
#' functionalities. For example, 3 of those packages can export to LaTeX. To
#' change the default backend used for a specific file format, you can use
#' the `options` function:
#'
#' `options(modelsummary_factory_html = 'kableExtra')`
#' 
#' `options(modelsummary_factory_latex = 'gt')`
#' 
#' `options(modelsummary_factory_word = 'huxtable')`
#' 
#' `options(modelsummary_factory_png = 'gt')`
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
#' `options(modelsummary_get = "broom")`
#' 
#' `options(modelsummary_get = "easystats")`
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
