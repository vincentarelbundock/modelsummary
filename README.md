# modelsummary: Beautiful, customizable, publication-ready model summaries in R.

[![Travis-CI Build Status](https://travis-ci.org/vincentarelbundock/modelsummary.svg?branch=master)](https://travis-ci.org/vincentarelbundock/modelsummary)

The `modelsummary` package for `R` produces beautiful, customizable, publication-ready tables to summarize statistical models. Results from several models are presented side-by-side, with uncertainty estimates in parentheses (or brackets) underneath coefficient estimates. Tables can be saved to HTML, LaTeX and RTF (MS Word-ready) formats, or they can be fed to a dynamic report pipeline like `knitr` or `Sweave`.

<img src="https://raw.githubusercontent.com/vincentarelbundock/modelsummary/master/examples/complex_table.png" width="40%">

# Table of contents

+ [Sales pitch](https://github.com/vincentarelbundock/modelsummary#sales-pitch)
+ [Installation](https://github.com/vincentarelbundock/modelsummary#installation)
+ [A simple example](https://github.com/vincentarelbundock/modelsummary#a-simple-example)
+ [Customizing your tables](https://github.com/vincentarelbundock/modelsummary#customizing-your-tables)
    * [Uncertainty estimates: SE, p, t, CI](https://github.com/vincentarelbundock/modelsummary#uncertainty-estimates-se-t-p-ci)
    * [Titles and subtitles](https://github.com/vincentarelbundock/modelsummary#titles-and-subtitles)
    * [Group columns (spanning labels)](https://github.com/vincentarelbundock/modelsummary#column-groups-spanning-labels)
    * [Notes](https://github.com/vincentarelbundock/modelsummary#notes)
    * [Rename, reorder, and subset](https://github.com/vincentarelbundock/modelsummary#rename-reorder-and-subset)
    * [Stars](https://github.com/vincentarelbundock/modelsummary#stars-statistical-significance-markers)
    * [Digits, rounding, exponential notation](https://github.com/vincentarelbundock/modelsummary#digits-rounding-exponential-notation)
    * [Colors and styles](https://github.com/vincentarelbundock/modelsummary#colors-and-styles)
    * [Fancy text with markdown: bold, italics, etc.](https://github.com/vincentarelbundock/modelsummary#fancy-text-with-markdown-bold-italics-etc)
    * [Font size](https://github.com/vincentarelbundock/modelsummary#font-size)
    * [Add rows manually](https://github.com/vincentarelbundock/modelsummary#add-rows-manually)
    * [Images](https://github.com/vincentarelbundock/modelsummary#images)
+ [A complex example](https://github.com/vincentarelbundock/modelsummary#a-complex-example)
+ [Other useful features](https://github.com/vincentarelbundock/modelsummary#other-useful-features)
    * [Output formats](https://github.com/vincentarelbundock/modelsummary#output-formats)
    * [Dynamic documents with knitr](https://github.com/vincentarelbundock/modelsummary#dynamic-documents-with-knitr)
    * [Unsupported models and custom tidiers](https://github.com/vincentarelbundock/modelsummary#unsupported-models-and-custom-tidiers)
    * [Pooled multiple imputation results](https://github.com/vincentarelbundock/modelsummary#pooled-multiple-imputation-results)
    * [Power users](https://github.com/vincentarelbundock/modelsummary#power-users)
+ [Alternative summary table packages for R](https://github.com/vincentarelbundock/modelsummary#alternative-summary-table-packages-for-r)

# Sales pitch

Here are a few benefits of `modelsummary` over some [alternative packages](https://github.com/vincentarelbundock/modelsummary#alternative-summary-table-packages-for-r):

* Customizability
    - Tables are endlessly customizable, thanks to the power of the [`gt` package.](https://gt.rstudio.com) In this README, you will find tables with colored cells, weird text, spanning column labels, row groups, titles and subtitles, global footnotes, cell-specific footnotes, significance stars, etc. This only scratches the surface of possibilities. For more, see [gt.rstudio.com](https://gt.rstudio.com) and the [Power Users](https://github.com/vincentarelbundock/modelsummary#power-users) section of this README.
* Flexibility
    - Tables can be saved to html, rtf, jpeg, png, pdf, or LaTeX files. (Coming soon: TXT/ASCII, and more.)
* Integration
    - `modelsummary` is extremely well integrated with RStudio. When you type `msummary(models)`, the summary table immediately appears in the Viewer window.
* Transparency, replicability, and automation
    - By combining `knitr` and `modelsummary`, you can easily produce beautiful, replicable, and automated documents and reports. [Click here for details.](https://github.com/vincentarelbundock/modelsummary#dynamic-documents-with-knitr)
* Community
    - `modelsummary` does not try to do everything. It leverages the incredible work of the `R` community by building on top of the popular `broom` package. Thanks to the `broom` team, `modelsummary` already supports dozens of model types out of the box. Most importantly, as `broom` and `gt` improve, `modelsummary` also improves.
* Reliability
    - `modelsummary` is developed using a suite of unit tests. It (probably) won't break.
* Simplicity
    - By using the `broom` and `gt` packages for key operations, `modelsummary` has a massively simplified codebase. This should improve long term code maintainability, and allow contributors to participate through GitHub.

CFITCRS!

At the `modelsummary` factory, we are *serious* about customizability. Are your bored of regression tables with good ol' "Intercept"? If so, we have [a solution for you:](https://github.com/vincentarelbundock/modelsummary#images)

<img src="https://raw.githubusercontent.com/vincentarelbundock/modelsummary/master/examples/squirrel_table.png" width="40%">

# Installation

The `gt` and `modelsummary` packages are not available on CRAN yet. You can install them from github:

```r
library(remotes)
remotes::install_github('rstudio/gt')
remotes::install_github('vincentarelbundock/modelsummary')
```

Make sure you also install `tidyverse`, as `modelsummary` depends on a lot of its packages (e.g., `stringr`, `dplyr`, `tidyr`, `purrr`):

```r
install.packages('tidyverse')
```

# A simple example

Load packages and download some data from the [RDatasets](https://vincentarelbundock.github.io/Rdatasets/) repository. Then, estimate 5 different models and store them in a named list. The name of each model in that list will be used as a column label:

```r
library(MASS)
library(modelsummary)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url) 
dat$Clergy <- ifelse(dat$Clergy > 40, 1, 0) # binary variable for logit model

models <- list()
models[['OLS 1']] <- lm(Literacy ~ Crime_prop + Infants, dat)
models[['NBin 1']] <- glm.nb(Literacy ~ Crime_prop + Donations, dat)
models[['OLS 2']] <- lm(Desertion ~ Crime_prop + Infants, dat)
models[['NBin 2']] <- glm.nb(Desertion ~ Crime_prop + Donations, dat)
models[['Logit 1']] <- glm(Clergy ~ Crime_prop + Infants, dat, family = binomial())
```

Produce a simple table:

```r
msummary(models)
```

Of course, `modelsummary` can also summarize single models:

```r
mod <- lm(Clergy ~ Crime_prop, data = dat)
msummary(mod)
```

# Customizing your tables

## Uncertainty estimates: SE, t, p, CI

`modelsummary` prints an uncertainty estimate in parentheses below the corresponding coefficient estimate. The `statistic` argument must be a string which is equal to `conf.int` or to one of the columns produced by the `broom::tidy` function. When using `conf.int`, users can specify a confidence level with the `conf_level` argument.

```r
msummary(models, statistic = 'std.error')
msummary(models, statistic = 'p.value')
msummary(models, statistic = 'statistic')
msummary(models, statistic = 'conf.int', conf_level = .99)
```
You can override the uncertainty estimates in a number of ways. First, you can specify a function that produces variance-covariance matrices:

```r
library(sandwich)
msummary(models, statistic_override = vcovHC, statistic = 'p.value')
```

You can supply a list of functions of the same length as your model list:

```r
msummary(models, 
   statistic_override = list(vcov, vcovHC, vcovHAC, vcovHC, vcov))
```

You can supply a list of named variance-covariance matrices:

```r
vcov_matrices <- lapply(models, vcovHC)
msummary(models, statistic_override = vcov_matrices)
```

You can supply a list of named vectors:

```r
custom_stats <- list(`OLS 1` = c(`(Intercept)` = 2, Crime_prop = 3, Infants = 4), 
                     `NBin 1` = c(`(Intercept)` = 3, Crime_prop = -5, Donations = 3),
                     `OLS 2` = c(`(Intercept)` = 7, Crime_prop = -6, Infants = 9), 
                     `NBin 2` = c(`(Intercept)` = 4, Crime_prop = -7, Donations = -9),
                     `Logit 1` = c(`(Intercept)` = 1, Crime_prop = -5, Infants = -2))
msummary(models, statistic_override = custom_stats)
```

## Titles and subtitles

You can add titles and subtitles to your table as follows:

```r
msummary(models, 
   title = 'This is a title for my table.',
   subtitle = 'And this is the subtitle.')
```

## Notes

Add notes to the bottom of your table:

```r
msummary(models, 
   notes = list('Text of the first note.', 
                'Text of the second note.'))
```

Add numbered footnotes to a column, a row, or a cell:

```r
library(gt)
msummary(models) %>% 
    tab_footnote(                                                                                                                                                                                  
        footnote = md("This is a **very** important model, so we are pointing it out in a column-specific footnote."),                        
        locations = cells_column_labels(columns = vars(`OLS 1`))) %>%  
    tab_footnote(                                                                                                                                                                               
        footnote = "This is the variable of interest.",                                                                                           
        locations = cells_stub(rows = vars(Infants))) %>%                                                                                                                                      
    tab_footnote(                                                                                                                      
        footnote = "Most important model + most important variable = most important estimate.",
        locations = cells_data(columns = vars(`OLS 1`), rows = vars(Infants)))
```

## Rename, reorder, and subset

### Coefficient estimates

The `coef_map` argument is a named vector which allows users to rename, reorder, and subset coefficient estimates. Values of this vector correspond to the "clean" variable name. Names of this vector correspond to the "raw" variable name. The table will be sorted in the order in which terms are presented in `coef_map`. Coefficients which are *not* included in `coef_map` will be excluded from the table.

```r
cm <- c('Crime_prop' = 'Crime / Population',
        'Donations' = 'Donations',
        '(Intercept)' = 'Constant')
msummary(models, coef_map = cm)
```

An alternative mechanism to subset coefficients is to use the `coef_omit` argument. This string is a regular expression which will be fed to `stringr::str_detect` to detect the variable names which should be excluded from the table.

```r
msummary(models, coef_omit = 'Intercept|Donation')
```

### Goodness-of-fit and other statistics

`gof_omit` is a regular expression which will be fed to `stringr::str_detect` to detect the names of the statistics which should be excluded from the table.

```r
msummary(models, gof_omit = 'DF|Deviance')
```

A more powerful mechanism is to supply a `data.frame` (or `tibble`) through the `gof_map` argument. This data.frame must include 4 columns:

1. `raw`: a string with the name of a column produced by `broom::glance(model)`.
2. `clean`: a string with the "clean" name of the statistic you want to appear in your final table.
3. `fmt`: a string which will be used to round/format the string in question (e.g., `"%.3f"`). This follows the same standards as the `fmt` argument in `?modelsummary`.
4. `omit`: `TRUE` if you want the statistic to be omitted from your final table.

You can see an example of a valid data frame by typing `modelsummary::gof_map`. This is the default data.frame that `modelsummary` uses to subset and reorder goodness-of-fit statistics. As you can see, `omit == TRUE` for quite a number of statistics. You can include setting `omit == FALSE`: 

```r
gm <- modelsummary::gof_map
gm$omit <- FALSE
msummary(models, gof_map = gm)
```

The goodness-of-fit statistics will be printed in the table in the same order as in the `gof_map` data.frame.

Notice the subtle difference between `coef_map` and `gof_map`. `coef_map` works as a "white list": any coefficient not explicitly entered will be omitted from the table. `gof_map` works as a "black list": statistics need to be explicitly marked for omission.

## Column groups (spanning labels)

Create spanning labels to group models (columns):

```r
msummary(models) %>%
    gt::tab_spanner(label = 'Literacy', columns = c('OLS 1', 'NBin 1')) %>%
    gt::tab_spanner(label = 'Desertion', columns = c('OLS 2', 'NBin 2')) %>%
    gt::tab_spanner(label = 'Clergy', columns = 'Logit 1')
```

## Stars: Statistical significance markers

Some people like to add "stars" to their model summary tables to mark statistical significance. The `stars` argument can take three types of input:

1. `NULL` omits any stars or special marks (default)
2. `TRUE` uses these default values: `* p < 0.1, ** p < 0.05, *** p < 0.01`
3. Named numeric vector for custom stars.

```r
msummary(models)
msummary(models, stars = TRUE) 
msummary(models, stars = c('+' = .1, '*' = .01)) 
```

Whenever `stars != NULL`, `modelsummary` adds a note at the bottom of the table automatically. If you would like to omit this note, just use the `stars_note` argument:

```r
msummary(models, stars = TRUE, stars_note = FALSE) 
```

If you want to create your own stars description, you can add custom notes with the [`notes`](https://github.com/vincentarelbundock/modelsummary#notes) argument.

## Digits, rounding, exponential notation

The `fmt` argument defines how numeric values are rounded and presented in the table. This argument follows the `sprintf` C-library standard. For example,

* `%.3f` will keep 3 digits after the decimal point, including trailing zeros.
* `%.5f` will keep 5 digits after the decimal point, including trailing zeros.
* Changing the `f` for an `e` will use the exponential decimal representation.

Most users will just modify the `3` in `%.3f`, but this is a very powerful system, and all users are encouraged to read the details: `?sprintf`

```r
msummary(models, fmt = '%.7f')
```

## Colors and styles

The power of the `gt` package makes `modelsummary` tables endlessly customizable. For instance, we can color columns and cells, and present values in bold or italics:

```r
msummary(models) %>%
    tab_style(style = cells_styles(bkgd_color = "lightcyan",
                                   text_weight = "bold"),
              locations = cells_data(columns = vars(`OLS 1`))) %>%
    tab_style(style = cells_styles(bkgd_color = "#F9E3D6",
                                   text_style = "italic"),
              locations = cells_data(columns = vars(`NBin 2`),
                                     rows = 2:6))
```

<img src="https://raw.githubusercontent.com/vincentarelbundock/modelsummary/master/examples/colors.png" width="50%">

## Fancy text with markdown: bold, italics, etc.

Thanks to `gt`, `modelsummary` accepts markdown indications for emphasis and more:

```r
msummary(models, 
   title = md('This is a **bolded series of words.**'),
   notes = list(md('And an *emphasized note*.')))
```

## Font size

This will produce a table with extra large row labels and extra small coefficient estimates.
```r
library(gt)
msummary(models) %>%
    tab_style(style = cells_styles(text_size = 'x-large'),
              locations = cells_stub()) %>%
    tab_style(style = cells_styles(text_size = 'x-small'),
              locations = cells_data())
```

Note that `gt`'s `tab_style` function is more developed for HTML output than for RTF or LaTeX, so some styling options may not be availble yet. The `gt` package is under heavy development, so feel free to file an issue on github if you have a special request, and stay tuned for more!

## Add rows manually

Use the `add_rows` argument to add rows manually to the bottom of the table.

```r
row1 <- c('Custom row 1', 'a', 'b', 'c', 'd', 'e')
row2 <- c('Custom row 2', 5:1)
msummary(models, add_rows = list(row1, row2))
```

## Images


Insert images in your tables using the `gt::text_transform` and `gt::local_image` functions.

```r
library(gt)
msummary(models) %>%
    text_transform(
        locations = cells_data(columns = 1, rows = 1),
        fn = function(x) {local_image(file = "examples/squirrel.png", height = 120)}
    )
```

# A complex example

This is the code I used to generate the "complex" table posted at the top of this README.

```r
library(gt)
cm <- c('Crime_prop' = 'Crime / Population',
        'Donations' = 'Donations',
        'Infants' = 'Infants',
        '(Intercept)' = 'Constant')
msummary(models,
   coef_map = cm,
   stars = TRUE,
   gof_omit = "Deviance",
   title = 'Summarizing 5 statistical models using the `modelsummary` package for `R`.',
   subtitle = 'Models estimated using the Guerry dataset.',
   notes = c('First custom note to contain text.',
             'Second custom note with different content.')) %>%
   # add spanning labels
   tab_spanner(label = 'Literacy', columns = c('OLS 1', 'NBin 1')) %>%
   tab_spanner(label = 'Desertion', columns = c('OLS 2', 'NBin 2')) %>%
   tab_spanner(label = 'Clergy', columns = 'Logit 1') %>%
   # footnotes
   tab_footnote(
       footnote = md("This is a **very** important model, so we are pointing it out in a column-specific footnote."),                        
       locations = cells_column_labels(columns = vars(`OLS 1`))) %>%  
    tab_footnote(                                                                                                                                                                               
        footnote = "This is the variable of interest.",                                                                                           
        locations = cells_stub(rows = vars(Infants))) %>%                                                                                                                                      
    tab_footnote(                                                                                                                      
        footnote = "Most important model + most important variable = most important estimate.",
        locations = cells_data(columns = vars(`OLS 1`), rows = vars(Infants))) %>%   
    # color and bold
    tab_style(style = cells_styles(text_color = "red", text_weight = "bold"),
              locations = cells_data(columns = vars(`OLS 1`), rows = vars(Infants)))
```

# Other useful features

## Output formats

To save a table to file, use the `filename` argument. `modelsummary` guesses the output format based on the `filename` extension. The supported extensions are: `.tex`, `.rtf`, `.html` (ASCII/Text tables coming soon).

```r
msummary(models, filename = 'table.tex')
msummary(models, filename = 'table.rtf')
msummary(models, filename = 'table.html')
msummary(models, filename = 'table.jpeg')
```

If `filename` is not specified, `modelsummary` returns a `gt` object which can be further customized and rendered by the relevant functions in the `gt` package, such as `as_raw_html`, `as_latex`, or `as_rtf`. RStudio renders the html version of this object automatically.

*Warning*: When creating complex tables by chaining multiple `gt` functions with the `%>%` pipe operator, the `filename` argument will not work. The problem is that `modelsummary` is trying to write-to-file immediately at the main `msummary()` call, before the rest of the functions in the chain are executed. In that case, it is better to use `gt::gtsave` explicitly at the very end of your chain. For example, 

```r
msummary(models) %>%
       gt::tab_spanner(label = 'Literacy', columns = c('OLS 1', 'NBin 1')) %>%
       gt::tab_spanner(label = 'Desertion', columns = c('OLS 2', 'NBin 2')) %>%
       gt::tab_spanner(label = 'Clergy', columns = 'Logit 1') %>%
       gt::gtsave('table.tex')
```

## Dynamic documents with `knitr`

You can use `knitr` and `modelsummary` to create dynamic documents with nice summary tables. When knitting in html format, adding a `msummary(models)` call to a code chunk should work out of the box.

When knitting to PDF output, things are slightly different. Indeed, the `gt` output functionality for LaTeX is still in development and it is somewhat limited. To avoid common sources of compilation errors, and to allow users to use `\label{}`, `modelsummary` includes the `knit_latex` function. To knit to PDF, simply use:

```r
msummary(models, title = 'Model summary') %>% 
    knit_latex(label = 'tab:example')
```

My goal is to deprecate `knit_latex` when `gt` LaTeX export features improve. 

Here are two minimal working examples of markdown files which can be converted to HTML or PDF using the `knitr` package. Just open one the `.Rmd` files in RStudio and click the "Knit" button:

* [markdown_to_pdf.Rmd](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_pdf.Rmd) / [markdown_to_pdf.pdf](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_pdf.pdf) 
* [markdown_to_html.Rmd](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_html.Rmd) / [markdown_to_html.html](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_html.html) 

## Unsupported models and custom tidiers

`modelsummary` relies on two functions from the `broom` package to extract model information: `tidy` and `glance`. If `broom` doesn't support the type of model you are trying to summarize, `modelsummary` won't support it out of the box. Thankfully, it is extremely easy to add support for most models using custom methods.

For example, models produced by the `MCMCglmm` package are not currently supported by `broom`. To add support, you simply need to create a `tidy` and a `glance` method:

```r
# load packages and data
library(modelsummary)
library(MCMCglmm)
data(PlodiaPO)

# add custom functions to extract estimates (tidy) and goodness-of-fit (glance) information
tidy.MCMCglmm <- function(object, ...) {
    s <- summary(object, ...)
    ret <- tibble::tibble(term = row.names(s$solutions),
                          estimate = s$solutions[, 1],
                          conf.low = s$solutions[, 2],
                          conf.high = s$solutions[, 3])
    ret
}
glance.MCMCglmm <- function(object, ...) {
    ret <- tibble::tibble(dic = object$DIC,
                          n = nrow(object$X))
    ret
}

# estimate a simple model
model <- MCMCglmm(PO ~ 1 + plate, random = ~ FSfamily, data = PlodiaPO, verbose=FALSE, pr=TRUE)

# summarize the model
msummary(model, statistic = 'conf.int')
```

Two important things to note. First, the methods are named `tidy.MCMCglmm` and `glance.MCMCglmm` because the model object I am trying to summarize is of class `MCMCglmm`. You can find the class of a model by running: `class(model)`.

Second, in the example above, we used the `statistic = 'conf.int'` argument. This is because the `tidy` method produces `conf.low` and `conf.high` columns. In most cases, users will define `std.error` column in their custom `tidy` methods, so the `statistic` argument will need to be adjusted.

If you create new `tidy` and `glance` methods, please consider contributing them to `broom` so that the rest of the community can benefit from your work: https://github.com/tidymodels/broom

## Pooled multiple imputation results

`modelsummary` can pool and display analyses on several datasets imputed using the `mice` package. For example:

```r
library(mice)

# Create a new dataset with missing values
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
tmp <- read.csv(url)[, c('Clergy', 'Donations', 'Literacy')]
tmp$Clergy[sample(1:nrow(tmp), 3)] <- NA
tmp$Donations[sample(1:nrow(tmp), 3)] <- NA
tmp$Literacy[sample(1:nrow(tmp), 3)] <- NA

# Impute dataset 5 times
tmp <- mice(tmp, m = 5, printFlag = FALSE, seed = 1024)

# Estimate models
mod <- list()
mod[[1]] <- with(tmp, lm(Clergy ~ Donations))
mod[[2]] <- with(tmp, lm(Clergy ~ Donations + Literacy))

# Summarize
msummary(mod, statistic = 't')
msummary(mod, statistic = 'ubar')
```

The `statistic` argument can take any column name in the tidy data frame obtained by:

```r
generics::tidy(mod[[1]])
```


## Power users

The `gt` package allows a bunch more customization and styling. Power users can use `modelsummary`'s `extract` function to produce a tibble which can easily be fed into `gt`.

```r
> modelsummary::extract(models)
# A tibble: 21 x 8
   group     term        statistic `OLS 1` `NBin 1` `OLS 2` `NBin 2` `Logit 1`
   <chr>     <chr>       <chr>     <chr>   <chr>    <chr>   <chr>    <chr>
 1 estimates (Intercept) estimate  64.114  4.218    57.331  4.384    1.006
 2 estimates (Intercept) statistic (5.247) (0.144)  (8.315) (0.233)  (0.710)
 3 estimates Crime_prop  estimate  -0.002  -0.000   -0.002  -0.000   -0.000
 4 estimates Crime_prop  statistic (0.001) (0.000)  (0.001) (0.000)  (0.000)
 5 estimates Infants     estimate  -0.001  ""       0.000   ""       -0.000
 6 estimates Infants     statistic (0.000) ""       (0.000) ""       (0.000)
 7 estimates Donations   estimate  ""      -0.000   ""      -0.000   ""
 8 estimates Donations   statistic ""      (0.000)  ""      (0.000)  ""
 9 gof       R2          ""        0.237   ""       0.073   ""       ""
10 gof       Adj.R2      ""        0.218   ""       0.051   ""       ""
# … with 11 more rows
```

# Alternative summary table packages for R

There are several excellent alternative summary table packages for R:

* [texreg](https://cran.r-project.org/package=texreg)
* [huxtable](https://cran.r-project.org/package=huxtable)
* [stargazer](https://cran.r-project.org/package=stargazer)
* [apsrtable](https://cran.r-project.org/package=apsrtable)

