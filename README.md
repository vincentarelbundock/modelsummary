# modelsummary <img src="https://user-images.githubusercontent.com/987057/82849698-05ba5700-9ec7-11ea-93a0-67dcd9151848.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/vincentarelbundock/modelsummary.svg?branch=master)](https://travis-ci.org/vincentarelbundock/modelsummary)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/vincentarelbundock/modelsummary?branch=master&svg=true)](https://ci.appveyor.com/project/vincentarelbundock/modelsummary)
<!-- badges: end -->

`modelsummary` creates beautiful and customizable tables to summarize statistical models in `R`. 

Results from several models are presented side-by-side. Tables can be echoed to the `R` console or viewed in the `RStudio` Viewer. They can be saved to HTML, PDF, Text/Markdown, LaTeX, RTF (Microsoft Word-compatible), JPG, and PNG formats. Tables can be integrated in dynamic document pipelines like `Rmarkdown`, `knitr`, or `Sweave`.

These two tables were created using `modelsummary` without any manual editing at all. The first is an HTML table. The second is a LaTeX table.

<center>
<img src="https://user-images.githubusercontent.com/987057/82853752-90558300-9ed4-11ea-88af-12cf20cb367f.png" width="40%">
<img src="https://user-images.githubusercontent.com/987057/82855711-0a3c3b00-9eda-11ea-8a81-1eebfbb7cb73.png" width="40%">
</center>

# Contents

+ [Why should I use `modelsummary`](#why-should-i-use-modelsummary)
+ [Installation](#installation)
+ [Getting started](#getting-started)
+ [Saving and viewing: output formats](#saving-and-viewing-output-formats)
+ [Customizing tables](https://vincentarelbundock.github.io/modelsummary/articles/customization.html)
    - [Information with `modelsummary`](https://vincentarelbundock.github.io/modelsummary/articles/customization.html#information-with-modelsummary)
    - [Appearance with `gt`: HTML, RTF (Microsoft Word-compatible), JPG, PNG](https://vincentarelbundock.github.io/modelsummary/articles/customization.html#appearance-gt)
    - [Appearance with `kableExtra`: HTML, LaTeX, PDF, Markdown/Text](https://vincentarelbundock.github.io/modelsummary/articles/customization.html#appearance-kableextra)
+ [Dynamic documents with `Rmarkdown` and `knitr`](https://vincentarelbundock.github.io/modelsummary/articles/rmarkdown.html)
+ [Advanced options](https://vincentarelbundock.github.io/modelsummary/articles/advanced.html)

# Why should I use `modelsummary`?

Here are a few benefits of `modelsummary` over some [alternative packages](#alternative-packages):

#### Easy

`modelsummary` is very easy to use. This simple call often suffices:

```r
library(modelsummary)

mod <- lm(y ~ x, dat)
msummary(mod)
```

The command above will automatically display a summary table in the `Rstudio` Viewer or in a web browser. A text-only version of the table can also be printed to the Console by typing:

```r
msummary(mod, "markdown")
```

#### Flexible

*Information*: The package offers many intuitive and powerful utilities to [customize the information](https://vincentarelbundock.github.io/modelsummary/articles/customization.html#information-with-modelsummary) reported in a summary table. You can rename, reorder, subset or omit parameter estimates; choose the set of goodness-of-fit statistics to include; display various “robust” standard errors or confidence intervals; add titles, footnotes, or source notes; insert stars or custom characters to indicate levels of statistical significance; or add rows with supplemental information about your models.

*Appearance*: Thanks to the [`gt`](https://gt.rstudio.com) and [`kableExtra`](https://haozhu233.github.io/kableExtra/) packages, the appearance of summary tables is endlessly customizable. The [appearance customization page](https://vincentarelbundock.github.io/modelsummary/articles/customization.html) shows tables with colored cells, weird text, spanning column labels, row groups, titles, source notes, footnotes, significance stars, and more. This only scratches the surface of possibilities.

*Output formats*: `modelsummary` tables can be saved to HTML, LaTeX, Text/Markdown, RTF (Word-Compatible), JPG, or PNG formats. They can also be inserted seamlessly in Rmarkdown documents to produce [automated documents and reports in PDF, HTML, or Microsoft Word-compatible formats.](https://vincentarelbundock.github.io/modelsummary/articles/rmarkdown.html)

#### Dangerous

`modelsummary` is dangerous! It allows users to do stupid stuff like [replacing their intercepts by squirrels.](https://vincentarelbundock.github.io/modelsummary/articles/customization.html#images)

<center><img src="https://user-images.githubusercontent.com/987057/82818916-7a60a780-9e6d-11ea-96ed-04fa92874a23.png" width="40%"></center>

#### Reliable

`modelsummary` is *reliably* dangerous! The package is developed using a [suite of unit tests.](https://github.com/vincentarelbundock/modelsummary/tree/master/tests/testthat), so it (probably) won't break.

#### Community

`modelsummary` does not try to do everything. Instead, it leverages the incredible work of the `R` community by building on top of the popular `broom`, `gt`, and `kableExtra` packages. Thanks to the `broom` team, `modelsummary` already supports dozens of model types out of the box. Thanks to the `gt` and `kableExtra` authors, `modelsummary` can produce beautiful tables in a large number of formats. 

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

We begin by loading the `modelsummary` package and by downloading data from the [RDatasets](https://vincentarelbundock.github.io/Rdatasets/) repository: 

```r
library(modelsummary)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url) 
```

We estimate a linear model and call the `msummary` function to display the results:

```r
mod <- lm(Donations ~ Crime_prop, data = dat)
msummary(mod)
```

<center><img src="https://user-images.githubusercontent.com/987057/82819815-08895d80-9e6f-11ea-8f78-93a62df204b3.png" width="15%"></center>

To summarize multiple models side-by-side, we store them in a list. If the items in that list are named, the names will be used as column labels:

```r
models <- list()
models[['OLS 1']] <- lm(Donations ~ Literacy + Clergy, data = dat)
models[['Poisson 1']] <- glm(Donations ~ Literacy + Commerce, family = poisson, data = dat)
models[['OLS 2']] <- lm(Crime_pers ~ Literacy + Clergy, data = dat)
models[['Poisson 2']] <- glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat)
models[['OLS 3']] <- lm(Crime_prop ~ Literacy + Clergy, data = dat)

msummary(models)
```

In `Rstudio`, the image below will be displayed automatically in the "Viewer" window. When running `R` from a terminal or from the basic `R` interface, this table should appear in your browser.

<center><img src="https://user-images.githubusercontent.com/987057/82816112-8eee7100-9e68-11ea-8bc7-30c0f2626539.png" width="40%"></center>

The same table can be printed in text-only format to the `R` Console:

```r
msummary(models, 'markdown')


|            |OLS 1      |Poisson 1   |OLS 2      |Poisson 2   |OLS 3      |
|:-----------|:----------|:-----------|:----------|:-----------|:----------|
|(Intercept) |7948.667   |8.241       |16259.384  |9.876       |11243.544  |
|            |(2078.276) |(0.006)     |(2611.140) |(0.003)     |(1011.240) |
|Clergy      |15.257     |            |77.148     |            |-16.376    |
|            |(25.735)   |            |(32.334)   |            |(12.522)   |
|Literacy    |-39.121    |0.003       |3.680      |-0.000      |-68.507    |
|            |(37.052)   |(0.000)     |(46.552)   |(0.000)     |(18.029)   |
|Commerce    |           |0.011       |           |0.001       |           |
|            |           |(0.000)     |           |(0.000)     |           |
|Num.Obs.    |86         |86          |86         |86          |86         |
|R2          |0.020      |            |0.065      |            |0.152      |
|Adj.R2      |-0.003     |            |0.043      |            |0.132      |
|AIC         |1740.8     |274160.8    |1780.0     |257564.4    |1616.9     |
|BIC         |1750.6     |274168.2    |1789.9     |257571.7    |1626.7     |
|Log.Lik.    |-866.392   |-137077.401 |-886.021   |-128779.186 |-804.441   |
```

# Saving and viewing: output formats

`modelsummary` can produce tables in these formats: 

* HTML 
* LaTeX 
* Text / Markdown / ASCII
* RTF (Microsoft Word-compatible)
* Images: JPG and PNG 
* `gt` table objects

Tables can be saved to file, or they can be previewed in the `R` console, the `Rstudio` Viewer, or a browser. Tables can be inserted in [dynamic `Rmarkdown` or `knitr` documents](https://vincentarelbundock.github.io/modelsummary/articles/rmarkdown.html), or they can be [customized](https://vincentarelbundock.github.io/modelsummary/articles/customization.html) using functions from the `gt` or `kableExtra` packages.

You can view tables in different formats by using the `output` argument:

```r
msummary(models)
msummary(models, output = 'html')
msummary(models, output = 'markdown')
msummary(models, output = 'latex')
msummary(models, output = 'gt')
```

You can save tables in even more formats by using the same `output` argument. `modelsummary` will use the file name extension to guess what kind of file to save. For example,

```r
msummary(models, output = 'table.html')
msummary(models, output = 'table.tex')
msummary(models, output = 'table.md')
msummary(models, output = 'table.txt')
msummary(models, output = 'table.png')
msummary(models, output = 'table.jpg')
msummary(models, output = 'table.rtf')
```

# Alternative packages

There are several excellent alternatives to draw model summary tables in `R`:

* [texreg](https://cran.r-project.org/package=texreg)
* [huxtable](https://cran.r-project.org/package=huxtable)
* [stargazer](https://cran.r-project.org/package=stargazer)
* [apsrtable](https://cran.r-project.org/package=apsrtable)
