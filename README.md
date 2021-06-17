# modelsummary <img src="https://user-images.githubusercontent.com/987057/82849698-05ba5700-9ec7-11ea-93a0-67dcd9151848.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![CRAN status](https://cranchecks.info/badges/flavor/release/modelsummary)](https://cran.r-project.org/web/checks/check_results_modelsummary.html)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/vincentarelbundock/modelsummary?branch=master&svg=true)](https://ci.appveyor.com/project/vincentarelbundock/modelsummary)
[![R build status](https://github.com/vincentarelbundock/modelsummary/workflows/R-CMD-check/badge.svg)](https://github.com/vincentarelbundock/modelsummary/actions)
[![codecov](https://codecov.io/gh/vincentarelbundock/modelsummary/branch/master/graph/badge.svg)](https://codecov.io/gh/vincentarelbundock/modelsummary)
[![status](https://tinyverse.netlify.com/badge/modelsummary)](https://CRAN.R-project.org/package=modelsummary)
[![](http://cranlogs.r-pkg.org/badges/grand-total/modelsummary?color=green)](https://cran.r-project.org/package=modelsummary)
<!-- badges: end -->

`modelsummary` creates tables and plots to summarize statistical models and data in `R`. 

The tables and plots produced by `modelsummary` are beautiful and highly customizable. They can be echoed to the `R` console or displayed in the `RStudio` Viewer. They can be saved to a wide variety of formats, including HTML, PDF, Text/Markdown, LaTeX, MS Word, RTF, JPG, and PNG. Tables can easily be embedded in dynamic documents with `Rmarkdown`, `knitr`, or `Sweave`. `modelsummary` supports *hundreds* of model types out-of-the-box. The look of your tables is infinitely customizable using external package such as `kableExtra`, `gt`, `flextable`, or `huxtable`.

`modelsummary` includes two families of functions:

1. Model Summary
    - `modelsummary`: Regression tables with side-by-side models.
    - `modelplot`: Coefficient plots.
2. Data Summary
    - `datasummary`: Powerful tool to create (multi-level) cross-tabs and data summaries.
    - `datasummary_crosstab`: Cross-tabulations.
    - `datasummary_balance`: Balance tables with subgroup statistics and difference in means (aka "Table 1").
    - `datasummary_correlation`: Correlation tables.
    - `datasummary_skim`: Quick overview ("skim") of a dataset.
    - `datasummary_df`: Turn dataframes into nice tables with titles, notes, etc.
      
The `modelsummary` website hosts a *ton* of examples. Make sure you click on the links at the top of this page: https://vincentarelbundock.github.io/modelsummary

The following tables and plots were created using `modelsummary`, without any manual editing at all:

| | |
|:-------------------------:|:-------------------------:|
|<img width="2406" src="https://user-images.githubusercontent.com/987057/82853752-90558300-9ed4-11ea-88af-12cf20cb367f.png">|<img width="2406" src="https://user-images.githubusercontent.com/987057/86512021-50839480-bdcc-11ea-893c-8c1e7a277895.png">
|<img width="2406" src="https://user-images.githubusercontent.com/987057/82855711-0a3c3b00-9eda-11ea-8a81-1eebfbb7cb73.png">|<img width="2406" src="https://user-images.githubusercontent.com/987057/85772292-b1cfa780-b6ea-11ea-8ae1-b95c6ddbf0a9.png">|
|<img width="2406" src="https://user-images.githubusercontent.com/987057/86502482-9eb77a00-bd71-11ea-80da-dc935c1fbd90.jpeg">|<img width="2406" src="https://user-images.githubusercontent.com/987057/86511490-cb967c00-bdc7-11ea-9d9b-0ef188840faf.png">
|<img width="2406" src="https://user-images.githubusercontent.com/987057/95397127-6d9a9880-08d0-11eb-8ee4-3cd181d55b32.png">|

# Why should I use `modelsummary`?

Here are a few benefits of `modelsummary` over some [alternative packages](#alternative-packages):

#### Easy

`modelsummary` is very easy to use. This simple call often suffices:

``` r
library(modelsummary)

mod <- lm(y ~ x, dat)
modelsummary(mod)
```

The command above will automatically display a summary table in the `Rstudio` Viewer or in a web browser. All you need is one word to change the output format. For example, a text-only version of the table can be printed to the Console by typing:

``` r
modelsummary(mod, output = "markdown")
```

Tables in Microsoft Word and LaTeX formats can be saved to file by typing:

``` r
modelsummary(mod, output = "table.docx")
modelsummary(mod, output = "table.tex")
```

#### Flexible

*Information*: The package offers many intuitive and powerful utilities to [customize the information](https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html) reported in a summary table. You can rename, reorder, subset or omit parameter estimates; choose the set of goodness-of-fit statistics to include; display various “robust” standard errors or confidence intervals; add titles, footnotes, or source notes; insert stars or custom characters to indicate levels of statistical significance; or add rows with supplemental information about your models.

*Appearance*: Thanks to the [`gt`](https://gt.rstudio.com), [`kableExtra`](https://haozhu233.github.io/kableExtra/), [`huxtable`](https://hughjonesd.github.io/huxtable/), and [`flextable`](https://davidgohel.github.io/flextable/) packages, the appearance of `modelsummary` tables is endlessly customizable. The [appearance customization page](https://vincentarelbundock.github.io/modelsummary/articles/appearance.html) shows tables with colored cells, weird text, spanning column labels, row groups, titles, source notes, footnotes, significance stars, and more.  This only scratches the surface of possibilities.

*Supported models*: Thanks to the [`broom`](https://broom.tidymodels.org/) and [`parameters`](https://easystats.github.io/parameters/), `modelsummary` supports *hundreds* of statistical models out-of-the-box. Installing other packages can extend the capabilities further (e.g., [`broom.mixed`](https://CRAN.R-project.org/package=broom.mixed)). It is also very easy to [add or customize your own models.](https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#adding-new-models-1)

*Output formats*: `modelsummary` tables can be saved to HTML, LaTeX, Text/Markdown, Microsoft Word, Powerpoint, RTF, JPG, or PNG formats.  They can also be inserted seamlessly in Rmarkdown documents to produce [automated documents and reports in PDF, HTML, RTF, or Microsoft Word formats.](https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#rmarkdown-and-knitr-1)

#### Dangerous

`modelsummary` is dangerous\! It allows users to do stupid stuff like [replacing their intercepts by squirrels.](https://vincentarelbundock.github.io/modelsummary/articles/appearance.html#gt-1)

<center>
<img src="https://user-images.githubusercontent.com/987057/82818916-7a60a780-9e6d-11ea-96ed-04fa92874a23.png" width="40%">
</center>

#### Reliable

`modelsummary` is *reliably* dangerous\! The package is developed using a [suite of unit tests](https://github.com/vincentarelbundock/modelsummary/tree/master/tests/testthat) with about 95% coverage, so it (probably) won’t break.

#### Community

`modelsummary` does not try to do everything. Instead, it leverages the incredible work of the `R` community. By building on top of the `broom` and `parameters` packages, `modelsummary` already supports hundreds of model types out-of-the-box. `modelsummary` also supports four of the most popular table-building and customization packages: `gt`, `kableExtra`, `huxtable`, and `flextable`. packages. By using those packages, `modelsummary` allows users to produce beautiful, endlessly customizable tables in a wide variety of formats, including HTML, PDF, LaTeX, Markdown, and MS Word.

One benefit of this community-focused approach is that when external packages improve, `modelsummary` improves as well. Another benefit is that leveraging external packages allows `modelsummary` to have a massively simplified codebase (relative to other similar packages). This should improve long term code maintainability, and allow contributors to participate through GitHub.

# Installation

You can install `modelsummary` from CRAN:

```r
install.packages('modelsummary')
```

If you want the very latest version, install it from Github:

```r
library(remotes)
remotes::install_github('vincentarelbundock/modelsummary')
```

# Getting started

There are a million ways to customize the tables and plots produced by `modelsummary`. In this Getting Started section we will only scratch the surface. For details, see the vignettes:

* `modelsummary`: https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html 
* `modelplot`: https://vincentarelbundock.github.io/modelsummary/articles/modelplot.html
* `datasummary`: https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html
* Appearance: https://vincentarelbundock.github.io/modelsummary/articles/appearance.html

To begin, load the `modelsummary` package and download data from the [Rdatasets archive](https://vincentarelbundock.github.io/Rdatasets/):

```r
library(modelsummary)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url) 
dat$Small <- dat$Pop1831 > median(dat$Pop1831)
```

Quick overview of the data:

```r
datasummary_skim(dat)
```

Balance table (aka "Table 1") with differences in means by subgroups:

```r
datasummary_balance(~Small, dat)
```

Correlation table:

```r
datasummary_correlation(dat)
```

Two variables and two statistics, nested in subgroups:

```r
datasummary(Literacy + Commerce ~ Small * (mean + sd), dat)
```

Estimate a linear model and display the results:

```r
mod <- lm(Donations ~ Crime_prop, data = dat)

modelsummary(mod)
```

Estimate five regression models, display the results side-by-side, and save them to a Microsoft Word document:

```r
models <- list(
  "OLS 1"     = lm(Donations ~ Literacy + Clergy, data = dat),
  "Poisson 1" = glm(Donations ~ Literacy + Commerce, family = poisson, data = dat),
  "OLS 2"     = lm(Crime_pers ~ Literacy + Clergy, data = dat),
  "Poisson 2" = glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat),
  "OLS 3"     = lm(Crime_prop ~ Literacy + Clergy, data = dat)
)

modelsummary(models, output = "table.docx")
```

# Alternative packages

There are several excellent alternatives to draw model summary tables in `R`:

  - [gtsummary](http://www.danieldsjoberg.com/gtsummary/)
  - [texreg](https://cran.r-project.org/package=texreg)
  - [stargazer](https://cran.r-project.org/package=stargazer)
  - [apsrtable](https://cran.r-project.org/package=apsrtable)
  - [huxtable](https://hughjonesd.github.io/huxtable/)
