# modelsummary 0.8.1.9000

Breaking change:

* Support for `dcolumn` for dot-aligned columns is deprecated. Use "d" in the `align` argument instead.

Other changes:

* LaTeX output: Numeric entries are wrapped in the `\num{}` function from the
  `siunitx` package by default. This produces much nicer formatting. This can be
  disabled with a global option. See `?modelsummary`
* The `align` argument accepts a "d" column for dot-alignment using the
  `siunitx` LaTeX package: `align="ldd"`.
* HTML tables display proper minus signs.
* New `escape` argument in most table-building functions.
* LaTeX output accepts the `threeparttable=TRUE` argument through `...`
* No more dependency on `tidyr`

`modelsummary`:

* `group`: The order of terms in the formula determines the order of rows/columns

`modelsummary_wide`: 

* Note: This function will eventually be deprecated
* Bugfix with statistic=NULL. 

`modelplot`:

* Preserves order of models in the user-supplied list

`datasummary_crosstab`:

* `statistic=NULL` produces a very basic crosstab

`datasummary_crosstab`:

* Default alignment "lrrrrr" consistent with other datasummary_* functions

# modelsummary 0.8.1

`modelsummary`:

* Disable stars footnote with `options("modelsummary_stars_note" = FALSE)`
* `longtable=TRUE` works for LaTeX output
* Interactions with ":" are no longer converted to "x" when `coef_map` or `coef_rename` are used.
* `group = model ~ term + group` is now supported.

`datasummary_skim`:

* `datasummary_skim("categorical")` keeps `NA` by default. Users can convert
  variables to factors before calling `datasummary_skim` to exclude `NA`.

Other:

* Improved warnings for bad calls: `modelsummary(model1, model2)`
* `gt` titles use the new `caption` argument in the `gt 0.3.0` function
* Bug fix: Overaggressive tests for glue strings prevented functions inside {}

# modelsummary 0.8.0

Breaking change:

* The default significance markers `stars=TRUE` have been updated to be
  consistent with the default output from base R (e.g., in summary.lm). The new
  significance thresholds are: 
  "+" p < 0.1, "*" p < 0.05, "**" p < 0.01, "***" p < 0.001

`datasummary_crosstab`:

* New function to produce cross-tabulations

`datasummary`:

* `N` is smart enough to return either the number of elements in a subset or the
  number of non-missing observations in a variable
  
`datasummary_balance`:

* Keeps `NA`s in factor variables by default. Users can convert their variables
  with the `factor()` function to omit `NA`s automatically.

`modelsummary`:

* themes can be set using global options (experimental)
* new vcov options: "bootstrap", "HAC", "NeweyWest", "Andrews",
  "panel-corrected", "weave", "outer-product"
* A valid `get_gof` (`glance`) is now optional.
* ... is pushed through to `sandwich`, which allows things like:
  `modelsummary(model, vcov = "bootstrap", R = 1000, cluster = "firm")`
 
Other:

* Jupyter notebook support via `output="jupyter"`
* Bug fixes

# modelsummary 0.7.0

`modelsummary`:

* new arguments for `modelsummary`: `group` and `group_map` for grouped
  parameters (e.g., outcome levels in multinomial logit or components of gamlss
  model).
* `dvnames()` makes it easy to get dependent variable column titles (thanks to @NickCH-K)
* `output="modelsummary_list"` to save a lightweight list-based representation
  of the table which can be saved and fed to `modelsummary` once more to get a
  full table.
* `vcov` adds a row to note the type of standard errors.
* `modelsummary` accepts a single model with multiple `vcov`s. 
* `get_gof` forwards ... to `model_performance`
* `coef_map` accepts unnamed vectors for easy subsetting
* `fixest::fixest_multi` support
* `options(modelsummary_get)` to set the order of extraction functions
  to use under the hood (broom vs. easystats vs. all)
* `metrics` argument of `performance::model_performance` is available
  via `modelsummary`'s ... ellipsis to limit the GOF statistics in
  Bayesian models.
* users can omit the stars legend note by using glue strings:
  `estimate="{estimate}{stars}"`
* output="html" can use `gt` by setting `options(modelsummary_html="gt")` 

`datasummary_correlation`:

* passes `...` forward
* new function: `datasummary_correlation_format`
* `datasummary_correlation`'s `method` argument accepts functions and
  "pearspear" (thanks to @joachim-gassen)
  
`datasummary`:

* `datasummary` functions and `rounding` accept ..., big.mark, etc.

`datasummary_skim`:

* now works with haven_labeled numeric
* faster tables with bayesian models. 
  
Bug fixes and lints

# modelsummary 0.6.6

* new output format: latex_tabular
* `tidy_custom` allows partial term matches
* `modelsummary(coef_rename)` accepts functions
* new function `coef_rename` for use in `modelsummary(coef_rename=coef_rename)`
* `modelplot` accepts `add_rows` to add reference categories
* informative error message when estimate or statistic is not available
* bug fixes

# modelsummary 0.6.5

* `statistic_override` becomes `vcov`
* vcov accepts shortcuts: "robust", "stata", "HC0", etc.
* vcov accepts formulas for clustered SEs: ~group
* modelsummary_wide has a new "stacking" argument
* html horizontal rule to separate estimates form gof
* gof_map accepts list of lists. only needs 3 columns.
* support officedown Rmd
* estimate accepts a vector for per model estimates
* options(modelsummary_default) can be markdown, html, latex
* bug: passing arguments through ...
* bug: stars and rounding

# modelsummary 0.6.4

* glue format for `estimate` and `statistic`
* easystats support for model info extraction
* deprecate statistic_vertical
* deprecate extract_models. Use modelsummary(output="dataframe") instead.
* modelplot pushes ... through to modelsummary(output="dataframe")
* datasummary_skim(type="dataset")
* gof_map omits by default
* datasummary_balance uses row percentages
* statistic_override does not require a list
* statistic_override accepts a single model
* N function for well formatted N in datasummary
* Bug fixes

# modelsummary 0.6.3

* new function: modelsummary_wide
* coef_omit and gof_omit use grepl(perl=TRUE)
* fmt accepts integer, string or function and respects options(OutDec=",")
* align argument for modelsummary
* align is more liberal to accept dcolumn alignment
* glance_custom methods for lfe and fixest 
* bug fixes

# modelsummary 0.6.2

* new argument: coef_rename
* new function: datasummary_df
* preserve term order in modelsummary
* refactor datasummary_balance
* datasummary_skim uses svg histograms instead of unicode
* removed 5 dependencies
* pass ... to kableExtra::kbl for more customization
* test improvements
* internal code style
* bug fixes

# modelsummary 0.6.1

* bug fixes

# modelsummary 0.6.0

* default HTML output factory is now kableExtra
* interaction ":" gsubbed by "\u00d7"
* dependencies: removed 1 depends, 3 imports, and 3 suggests
* word_document knitr works out-of-the-box
* bug fixes

# modelsummary 0.5.1

* glance_custom.fixest ships with modelsummary

# modelsummary 0.5.0

* datasummary
* datasummary_skim
* datasummary_balance
* datasummary_correlation
* modelplot
* allow duplicate model names
* bug: can't use coef_map with multiple statistics (thanks @sbw78)
* bug: wrong number of stars w/ statistic='p.value' (thanks @torfason)
* output='data.frame'. `extract` is no longer documented.

# modelsummary 0.4.1

* add_rows now accepts a data.frame with "position" and "section" columns
* add_rows_location is deprecated
* bug in sanity_output prevented overwriting files

# modelsummary 0.4.0

* huxtable support
* flextable support
* estimate argument
* fixest tidiers
* website and vignette improvements
* gof_map additions
* glance_custom
* tidy_custom

# modelsummary 0.3.0

* Out-of-the-box Rmarkdown compilation to HTML, PDF, RTF
* kableExtra output format for LaTeX and Markdown
* Support for `threeparttable`, colors, and many other LaTeX options
* Deprecated arguments: filename, subtitle
* Deprecated functions: clean_latex, knit_latex
* `pkgdown` website and doc improvements
* `mitools` tidiers
* New tests

# modelsummary 0.2.1

* Convenience function to render markdown in row/column labels
* bug: breakage when all GOF were omitted
* Clean up manual with @keywords internal
* bug: tidyr import

# modelsummary 0.2.0

* gt is now available on CRAN
* new latex_env argument for knit_latex and clean_latex
* bug when all gof omitted
* bug in statistic_override with functions
* bug caused by upstream changes in tab_style
* bug caused by upstream changes in filename='rtf'
* Allow multiple rows of uncertainty estimates per coefficient
* Preserve add_rows order
* Display uncertainty estimates next to the coefficient with statistic_vertical = FALSE
* Better clean_latex function
* Can display R2 and confidence intervals for mice-imputed lm-models
* Internal functions have @keywords internal to avoid inclusion in docs
* Statistic override accepts pre-formatted character vectors

# modelsummary 0.1.0

* Initial release (gt still needs to be installed from github)
