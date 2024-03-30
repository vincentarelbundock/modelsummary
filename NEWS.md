# News 

## 2.0.0

MAJOR BREAKING CHANGE: The default output format is now `tinytable` instead of `kableExtra`. Learn more about `tinytable` here:

https://vincentarelbundock.github.io/tinytable/

To revert to the previous behavior persistently, users can call:

library(modelsummary)
config_modelsummary(factory_default = "kableExtra")

Other breaking changes:

* The `statistic_override` argument was replaced by `vcov` over 1 year ago, with appropriate deprecation warnings. It is now fully removed.
* The `group` argument was replaced by `shape` several releases ago. It is now fully removed.
* `datasummary_skim()`
  - histograms are only available with the `tinytable` backend. This allows a lot of code simplification and more customization.
  - The order of arguments `type` and `output` is switched for consistency with other functions.
  - `histogram` argument is deprecated.

New features:

* `datasummary_skim()`:
  - New `type="all"` by default to display both numeric and categorical variables in a single table with distinct panels. This feature is only available with the `tinytable` backend (default).
  - `by` argument allows summarizing numeric variables by group.
  - `fun_numeric` argument accepts a list of functions to control the summary columns.
* `modelsummary()`:
  - `statistic` and `estimate` can be specified as named vectors to control the names of statistics when displayed in different columns using the `shape` argument. (Thanks to @mps9506 for bug report #722)
  - `modelsummary(panels, shape = "cbind")` automatically adds column-spanning labels when `panels` is a named nested list of models.
* `config_modelsummary()` gets a `startup_message` argument to silence the startup message persistently.
* Improved documentation and vignettes, providing clearer instructions and examples.
* Updated tests and snapshots to ensure reliability and consistency across changes.

Bug fixes:

* Fixed Issue #399: datasummary_balance() siunitx formatting.
* Fixed Issue #782: Useless warning in some `modelplot()` calls. Thanks to @iago-pssjd for the report and @florence-laflamme for the fix.
* Addressed various bugs and made optimizations for better performance and user experience.


## 1.4.5

* `tinytable` supports histograms in `datasummary_skim()`
* `config_modelsummary()` supports `tinytable` factory.


## 1.4.4

* Support the `tinytable` package as an output format ("factory"): https://vincentarelbundock.github.io/tinytable/
* Quarto: `md` output format is recognized.
* `options(modelsummary_factory_default)` is respected, even in qmd->md documents.

Bugs:

* Some omitted coefficients with `I()` operator in formulas. Issue #693.

## 1.4.3

Misc:

* Duplicate values in `shape` groups are removed automatically for cleaner labels.
* "Title" line no longer indented in markdown tables. Thanks to Ryan Briggs for report #671.

Bugs:

* Small p values were not displayed properly in HTML output using `kableExtra`. Issue #669.

## 1.4.2

New:

* Minimal support for `Typst` output, with auto-detection in Quarto documents.
* `strip` argument in `dvnames`.
* `s.value` statistic is now available whenever `p.value` is available. See Greenland (2019).
* `datasummary_skim()` now includes histograms in `gt` tables.

Bugs:

* GOF term names get escaped in LaTeX. Thanks to @shreyasgm for reviving Issue #546.
* Conflict with `furrr` generated errors for some models. Thanks to @sammo3182 for Issue #647.

## 1.4.1

New:

* `fmt_sci()` can now be used in the `fmt` argument for rounding with scientific notation.

Bugs:

* Group separators respect `add_rows` with `shape="rbind"`. Thanks to @lrose1 for Report #626.
* Bad column with horizontal models in `shape` and grouped estimates. Thanks to @iago-pssjd for Report #631.
* `coef_rename=TRUE` bug with grouped estimates. Thanks to @iago-pssjd for Report #631.
* Upstream issue #881 in `parameters` meant that `vcov` was no longer used for confidence intervals.

## 1.4.0

* Built-in support for markdown tables.
* Package no longer depends on `kableExtra`. Recommends an additional install for other formats.
* Persistent configuration of default output format: `config_modelsummary(factory_default = "gt")`
* `shape = "rcollapse"` and `shape = "rbind"`
* `glance_custom()` can drop GOF by assigning `NA`: https://stackoverflow.com/questions/75215355/assigning-different-coefficient-names-and-goodness-of-fit-statistics-to-differen
* When a `statistic` is not available, `modelsummary` prints an empty cell instead of returning an error.
* "\\label{tab:something}" works in `title` even when `escape=TRUE`
* Multiple `fixest_multi` objects supported.
* `options(modelsummary_future = FALSE)` disables `future` parallelism.

Bug fixes:

* `statistic=NULL` is now respected when `shape="rbind"`. Thanks to Panos Mavros for report #620.
* `get_estimates()` supports `vcov` string shortcuts and formulas. Thanks to @ethans-carl for report #605.
* Quarto and Rmarkdown documents include `situnix` in header automatically for decimal alignement with `align="ddd"` 
* `escape` is now respected by `modelsummary` with `shape="rbind"`. Thanks to @chickymonkeys for report #622.

## 1.3.0

Breaking change:

* The default column label style in `modelsummary()` has changed from "Model 1" to "(1)". The benefits are: labels are no longer in English by default; use less horizontal space; eliminate the "Model" redundancy. Unfortunately, this could break code in some edge cases where users rely on column names to manipulate tables. The old behavior can be restored by calling: `options(modelsummary_model_labels="model")`

New features:

* `shape="rbind"` to stack multiple regression tables and create "panels" with labelled groups of models.
* `fmt`: new helper functions for different formatting styles
  - `fmt = fmt_decimal(2)`: decimal digits
  - `fmt = fmt_decimal(digits = 2, pdigits = 4)`: decimal digits with p value-specific setting
  - `fmt = fmt_sprintf("%.3f")`: `sprintf()` decimal
  - `fmt = fmt_sprintf("%.3e")`: `sprintf()` scientific
  - `fmt = fmt_significant(3)`: significant digits
  - `fmt = fmt_statistic("estimate" = 2, "std.error" = 3)`: statistic-specific formatting
  - `fmt = fmt_term("(Intercept)" = 2, "hp" = 3)`: term-specific formatting
  - `fmt = fmt_identity()`: raw values
* New styles for default column labels in `modelsummary`, such as Roman Numerals or letters in parentheses.
  - Set the style with a global option: `options(modelsummary_model_labels = "roman")`
  - Supported styles: "model", "arabic", "letters", "roman", "(arabic)", "(letters)", "(roman)""
* `modelplot(draw = FALSE)` now returns a `p.value` column. This allows conditional aesthetics (see the `modelplot` vignette).
* Better integration with the `marginaleffects` package.

Bugs:

* Some `fixest` models returns useless "group.x" and "group.y" columns. Isse #591. Thanks to Adam Altmejd for the report.

## 1.2.0

Breaking change:

* With the `shape` and `output="dataframe"` arguments, there always used to be a `group` column. Now, this column has the same name as the variable in the `shape` formula ("response", "component", etc.).

New features:

* `shape` can include multiple groups.
* `coef_rename` can be an unnamed vector of length equal to the number of terms in the final table, obtained after `coef_map` and `coef_omit` are applied and models are merged.
* `coef_omit` accepts numeric indices. Positive values: coefficients to omit. Negative values: coefficients to keep.
* `datasummary_skim`: Increased maximum number of variables to 250.
* Quarto notebooks compile to Word and Markdown automatically.

Bug fixes:

* Order of notes preserved in some output format (Issue #577)

## 1.1.0

Breaking change:

* Requires `siunitx` version 3.0.25 LaTeX package.
* The `title` argument now respects the `escape` argument for all `kableExtra` output formats. This can break tables in which users manually escaped titles.

New features:

* "d" is accepted for decimal-alignment in the `align` argument for all `output` formats. `modelsummary(mod, align = "ld")`
* New `update_modelsummary()` function makes it easy to install the dev versions of `modelsummary` and its dependencies (mostly useful for Vincent and people who report bugs).
* Rounding: display at least one significant digit by default.
* Automatic renaming of `haven` labels in `modelsummary()`, `datasummary()`, `datasummary_skim()`
* Allow `output = "filename.csv"`
* Allow `output = "filename.xlsx"`
* `add_columns` argument supported in `modelsummary()`
* `datasummary_balance` supports the `stars` argument.
* Allow stars and confidence intervals with `align = "d"` column.

Bug fixes:

* In some locales, the HTML minus sign created problems in the output. We only use it in "known" locales.
* Many minor bug fixes

## 1.0.2

* Minor release to fix CRAN failure

## 1.0.1

* `shape` argument accepts interactions with the colon ":" character. This combines two columns into one, which can be useful to display terms and group names in a single column.
* Parallelization using `parallel::mclapply`. See `?modelsummary` 
* `modelsummary` no longer computes confidence intervals when not necessary, which can save some time. Also see: `conf_level=NULL`
* Added log likelihood to GOF for lm and glm models.
* Removed extraneous warnings
* Bug fixes

## 1.0.0

This first major release accompanies the publication of an article in the Journal of Statistical Software:

Arel-Bundock, Vincent (2022). "modelsummary: Data and Model Summaries in R." _Journal of Statistical Software_, *103*(1), 1-23. doi:10.18637/jss.v103.i01 <https://doi.org/10.18637/jss.v103.i01>.'

If you like `modelsummary`, please cite the JSS article and tell your friends about it.

Minor changes:

* `gof_map="all"` includes all available statistics. `gof_map="none"` excludes all statistics.
* Bug fixes

## 0.11.1

* Better printout for term names in mixed-effects models
* {brms} and {stanreg} models now extracted with `diagnostic=NULL` and `test=NULL` by default for speed.

## 0.11.0

Breaking changes:

* `modelsummary_wide` is no longer available. Use the `shape` argument of `modelsummary` instead.
* `modelsummary` now uses the `easystats` packages (`performance` and `parameters`) to extract estimates and goodness-of-fit statistics instead of `broom`. This can be reverted by setting a global option: `options(modelsummary_get="broom")`. This change aims to (1) increase consistency across models, (2) improve the developers' ability to push bug fixes upstream when necessary, and (3) improve support for mixed effects, bayesian, and GAM models. The two main drawbacks are: (a) The set of printed statistics may be slightly different from previous versions of `modelsummary` (b) The group identifiers used in the `shape` formula will also be different for certain models (e.g., in `nnet::multinom`, `y.level` becomes `response`). 

New features:

* The `shape` argument accepts a formula and can reshape information in myriad ways. Deprecates the `group` argument. Examples:
    - `~ statistic`: statistics are shown horizontally in distinct columns.
    - `model ~ term`: models in rows and terms in columns.
    - `term + y.level + statistic ~ model`: grouped coefficients for multivariate outcome in `nnet::multinom`
    - `y.level ~ model`: partial match is the same as the previous formula
* Format distinct statistics differently by passing a named list to `fmt`:
    - `modelsummary(mod, fmt = list(estimate = 2, std.error = 1, rmse = 4))`
* Use `glue` to apply functions to numeric values by setting `fmt = NULL`. Example:
    - `modelsummary(model, fmt = NULL, estimate = "{log(estimate)}")`
* Update for breaking changes after fixest 0.10.4

Bug fixes:

* `group_map` rename issue
* Residual standard error mistakenly labelled "RMSE" in `lm` models.
* `datasummary_skim` output to jpg should now works
* `escape` fixes

## 0.10.0

* New `exponentiate` argument for `modelsummary()` and `modelplot()`
* `gof_map` accepts a vector such as `c("rmse", "nobs", "r.squared")`
* Drop `rlang` dependency
* Bug fixes

## 0.9.6

`datasummary_balance`:

* Accepts `~ 1` as a formula to summarize all data.

Misc:

* documentation improvements
* RMSE included by default in models of class `lm`

## 0.9.5

`modelsummary`:

* `vcov` strings like `HC1` and `Robust` are now case-insensitive
* `gof_map` now accepts a data.frame or tibble with a `fmt` list-column which includes functions (see Examples in docs)
* `R2` is no longer computed by default for bayesian and mixed effects models. An informative one-time warning is printed about the `metrics` argument.

`datasummary_skim`:

* Histograms now work in Jupyter
* Bugfix: harmless error message is no longer printed

`kableExtra` factory:

* The `col.names` argument can now be passed to `kableExtra::kbl` through the ... ellipsis.

Misc:

* Many small improvements to the vignettes and docs
* `output = "github_document"` is now supported

## 0.9.4

* Bug fix: siunitx and rounding NA

## 0.9.3

`modelsummary`:

* F statistic takes into account `vcov` argument
* Support group = group ~ model + term

`datasummary_balance`:

* Weighted means and standard deviations are now supported. Counts and percentages are not, but raise a warning.

Misc:

* Bugfix: rounding in LaTeX w/ siunitx and NaN entries.
* output='jupyter' no longer prints an extraneous TRUE to the notebook

## 0.9.2

`modelsummary`:

* Improved `vcov` argument handling for `fixest` models (#357 by @grantmcdermott)
* Fix display of `fixest::i()` variables and interactions (#361 by @grantmcdermott)
* Consistent display of clustered SEs (#356, #363 and #366 by @grantmcdermott)

`datasummary_correlation`:

* `add_rows` and `add_columns` arguments are now available here.

Misc:

* Global options for output factories are renamed: `modelsummary_factory_default`, `modelsummary_factory_html`, etc.
* Hot fix for change in R-devel behavior or `intersect`

Bug fixes:

* `datasummary_balance`: escape variable names when `escape=TRUE`
* Blogdown LaTeX dependency bug when output is HTML

## 0.9.1

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

## 0.8.1

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

## 0.8.0

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

## 0.7.0

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
* output="html" can use `gt` by setting `options(modelsummary_factory_html="gt")` 

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

## 0.6.6

* new output format: latex_tabular
* `tidy_custom` allows partial term matches
* `modelsummary(coef_rename)` accepts functions
* new function `coef_rename` for use in `modelsummary(coef_rename=coef_rename)`
* `modelplot` accepts `add_rows` to add reference categories
* informative error message when estimate or statistic is not available
* bug fixes

## 0.6.5

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

## 0.6.4

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

## 0.6.3

* new function: modelsummary_wide
* coef_omit and gof_omit use grepl(perl=TRUE)
* fmt accepts integer, string or function and respects options(OutDec=",")
* align argument for modelsummary
* align is more liberal to accept dcolumn alignment
* glance_custom methods for lfe and fixest 
* bug fixes

## 0.6.2

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

## 0.6.1

* bug fixes

## 0.6.0

* default HTML output factory is now kableExtra
* interaction ":" gsubbed by "\u00d7"
* dependencies: removed 1 depends, 3 imports, and 3 suggests
* word_document knitr works out-of-the-box
* bug fixes

## 0.5.1

* glance_custom.fixest ships with modelsummary

## 0.5.0

* datasummary
* datasummary_skim
* datasummary_balance
* datasummary_correlation
* modelplot
* allow duplicate model names
* bug: can't use coef_map with multiple statistics (thanks @sbw78)
* bug: wrong number of stars w/ statistic='p.value' (thanks @torfason)
* output='data.frame'. `extract` is no longer documented.

## 0.4.1

* add_rows now accepts a data.frame with "position" and "section" columns
* add_rows_location is deprecated
* bug in sanity_output prevented overwriting files

## 0.4.0

* huxtable support
* flextable support
* estimate argument
* fixest tidiers
* website and vignette improvements
* gof_map additions
* glance_custom
* tidy_custom

## 0.3.0

* Out-of-the-box Rmarkdown compilation to HTML, PDF, RTF
* kableExtra output format for LaTeX and Markdown
* Support for `threeparttable`, colors, and many other LaTeX options
* Deprecated arguments: filename, subtitle
* Deprecated functions: clean_latex, knit_latex
* `pkgdown` website and doc improvements
* `mitools` tidiers
* New tests

## 0.2.1

* Convenience function to render markdown in row/column labels
* bug: breakage when all GOF were omitted
* Clean up manual with @keywords internal
* bug: tidyr import

## 0.2.0

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

## 0.1.0

* Initial release (gt still needs to be installed from github)
