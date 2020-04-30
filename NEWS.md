# 0.2.1: 2020-04-29

* Convenience function to render markdown in row/column labels
* bug: breakage when all GOF were omitted
* Clean up manual with @keywords internal
* bug: tidyr import

# 0.2.0: 2020-04-04

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
* Statistic override accepts pre-formated character vectors

# 0.1.0: 2019-06-30

* Initial release (gt still needs to be installed from github)
