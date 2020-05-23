# modelsummary: Beautiful, customizable, publication-ready model summaries in R.

[![Travis-CI Build Status](https://travis-ci.org/vincentarelbundock/modelsummary.svg?branch=master)](https://travis-ci.org/vincentarelbundock/modelsummary)

The `modelsummary` package for `R` produces beautiful, customizable, publication-ready tables to summarize statistical models. Results from several models are presented side-by-side, with uncertainty estimates in parentheses (or brackets) underneath coefficient estimates. Tables can be saved to HTML, LaTeX and RTF (MS Word-ready) formats, or they can be fed to a dynamic report pipeline like `knitr` or `Sweave`.

<img src="https://raw.githubusercontent.com/vincentarelbundock/modelsummary/master/examples/table_complex.png" width="40%">

# Table of contents

+ [Sales pitch](https://github.com/vincentarelbundock/modelsummary#sales-pitch)
+ [Installation](https://github.com/vincentarelbundock/modelsummary#installation)
+ [A simple example](https://github.com/vincentarelbundock/modelsummary#a-simple-example)
+ [Output formats: HTML, LaTeX, Text/Markdown, RTF, JPG, PNG, gt](https://github.com/vincentarelbundock/modelsummary#output-formats-html-latex-markdown-rtf-jpg-png-gt)
    - [Save table to file](https://github.com/vincentarelbundock/modelsummary#save-to-file)
    - [View table](https://github.com/vincentarelbundock/modelsummary#print-to-console)
    - [Cutomizing tables](https://github.com/vincentarelbundock/modelsummary#customizing-tables)
    - [`gt` vs. `kableExtra`](https://github.com/vincentarelbundock/modelsummary#gt-vs-kableextra)
    - [LaTeX](https://github.com/vincentarelbundock/modelsummary#latex)
    - [`rmarkdown`, `knitr`, and `Sweave`](https://github.com/vincentarelbundock/modelsummary#rmarkdown-knitr-and-sweave)
+ [Customizing your tables](https://github.com/vincentarelbundock/modelsummary#customizing-your-tables)
    * [Uncertainty estimates: SE, p, t, CI](https://github.com/vincentarelbundock/modelsummary#uncertainty-estimates-se-t-p-ci)
    * [Titles and subtitles](https://github.com/vincentarelbundock/modelsummary#titles-and-subtitles)
    * [Group columns (spanning labels)](https://github.com/vincentarelbundock/modelsummary#column-groups-spanning-labels)
    * [Notes](https://github.com/vincentarelbundock/modelsummary#notes)
    * [Rename, reorder, and subset](https://github.com/vincentarelbundock/modelsummary#rename-reorder-and-subset)
    * [Stars](https://github.com/vincentarelbundock/modelsummary#stars-statistical-significance-markers)
    * [Digits, rounding, exponential notation](https://github.com/vincentarelbundock/modelsummary#digits-rounding-exponential-notation)
    * [Extra tidy arguments (e.g., exponentiated coefficients)](https://github.com/vincentarelbundock/modelsummary#extra-tidy-arguments-eg-exponentiated-coefficients)
    * [Colors and styles](https://github.com/vincentarelbundock/modelsummary#colors-and-styles)
    * [Fancy text with markdown: bold, italics, etc.](https://github.com/vincentarelbundock/modelsummary#fancy-text-with-markdown-bold-italics-etc)
    * [Font size](https://github.com/vincentarelbundock/modelsummary#font-size)
    * [Add rows manually](https://github.com/vincentarelbundock/modelsummary#add-rows-manually)
    * [Images](https://github.com/vincentarelbundock/modelsummary#images)
+ [A complex example](https://github.com/vincentarelbundock/modelsummary#a-complex-example)
+ [Other useful features](https://github.com/vincentarelbundock/modelsummary#other-useful-features)
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

<img src="https://imgur.com/CQp4uXl.png" width="40%">

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

`modelsummary` relies heavily on the `gt` package, which is not available on CRAN yet. You can install it from github:

```r
remotes::install_github('rstudio/gt')
```

Make sure you also install `tidyverse`, as `modelsummary` depends on a lot of its packages (e.g., `stringr`, `dplyr`, `tidyr`, `purrr`):

```r
install.packages('tidyverse')
```

# A simple example

Load packages and download some data from the [RDatasets](https://vincentarelbundock.github.io/Rdatasets/) repository. Then, estimate 5 different models and store them in a named list. The name of each model in that list will be used as a column label:

```r
library(gt)
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

# Output formats: HTML, LaTeX, Markdown, RTF, JPG, PNG, gt

`modelsummary` can produce tables in the following formats: 

* HTML 
* LaTeX 
* Text / Markdown / ASCII
* RTF (Microsoft Word-compatible)
* .jpg and .png images 
* `gt` table objects

These output formats can be echoed to the `R` console, saved to file, or the tables can be further customized and post-processed using functions from the `gt` or `kableExtra` packages.

`modelsummary` can also insert tables in "dynamic" documents produced by `Rmarkdown`, `knitr`, or `Sweave`.

## Save to file

To save a table to file, use the `output` argument. `modelsummary` will use the file name extension to guess what kind of file to save. For example,

```r
msummary(models, output = 'table.html')
msummary(models, output = 'table.tex')
msummary(models, output = 'table.md')
msummary(models, output = 'table.txt')
msummary(models, output = 'table.png')
msummary(models, output = 'table.jpg')
msummary(models, output = 'table.rtf')
```

By default, `modelsummary` uses the `gt` package to save HTML, JPG, and PNG files.  It uses `kableExtra` to save LaTeX and Markdown files.

## View table

To print the table, set the `output` argument to "gt", "html", "latex", or "markdown" (the default "gt" returns a `gt` object and displays the associated HTML table). For example,

```r
msummary(models, output = 'latex')
msummary(models, output = 'html')
msummary(models, output = 'gt')
```

Using `output = "markdown"` prints a markdown/ASCII table to the console:

```r
> msummary(models, output = 'markdown')


|            |OLS 1    |NBin 1   |OLS 2    |NBin 2   |Logit 1 |
|:-----------|:--------|:--------|:--------|:--------|:-------|
|(Intercept) |64.114   |4.218    |57.331   |4.384    |1.006   |
|            |(5.247)  |(0.144)  |(8.315)  |(0.233)  |(0.710) |
|Crime_prop  |-0.002   |-0.000   |-0.002   |-0.000   |-0.000  |
|            |(0.001)  |(0.000)  |(0.001)  |(0.000)  |(0.000) |
|Infants     |-0.001   |         |0.000    |         |-0.000  |
|            |(0.000)  |         |(0.000)  |         |(0.000) |
|Donations   |         |-0.000   |         |-0.000   |        |
|            |         |(0.000)  |         |(0.000)  |        |
|Num.Obs.    |86       |86       |86       |86       |86      |
|R2          |0.237    |         |0.073    |         |        |
|Adj.R2      |0.218    |         |0.051    |         |        |
|AIC         |718.8    |720.2    |797.9    |803.2    |123.0   |
|BIC         |728.6    |730.0    |807.8    |813.1    |130.4   |
|Log.Lik.    |-355.382 |-356.106 |-394.974 |-397.620 |-58.498 |
msummary(models, output = 'markdown')
```

## Customizing tables

Both `gt` and `kableExtra` allow endless customization of HTML and LaTeX tables. To customize a table, users generally *post-process* the outcome of `modelsummary()` using functions from the `gt` or `kableExtra` package. For example, to add a note to the bottom of an HTML table, users can type:

```r
msummary(models) %>% tab_source_note('This is a note.')
```

Users are encouraged to read the documents of both packages to see which syntax they prefer. 

* [HTML tables with `gt`](https://gt.rstudio.com)
* [HTML tables with `kableExtra`](http://haozhu233.github.io/kableExtra/)

*Warning*: When users supply of file name to the `output` argument, the table is written immediately to file. This means that users cannot post-process and customize the resulting table using fuunctions from `gt` or `kableExtra`. To save a customized table, you should apply all the customization functions you need, and only then use `gt::gtsave` or `kableExtra::save_kable` to save the table to file. 

## `gt` vs. `kableExtra`

To build tables `modelsummary` relies on two external packages: `gt` and `kableExtra`. Each package has its own advantages and disadvantages. `gt` is extremely powerful for HTML tables, and it is extremely well integrated with the RStudio IDE. Typing `msummary(models)` produces an HTML table that is immediately visible in the `Rstudio` Viewer window. Unfortunately, `gt`'s support for LaTeX and PDF outputs for Rmarkdown documents is immature. In contrast, `kableExtra` has mature and full-featured LaTeX and PDF rendering functions. For this reason, `modelsummary` uses `gt` by default for HTML documents, and `kableExtra` by default for LaTeX documents.

You can override these defaults like this:

```r
# kableExtra HTML table
msummary(models, output = 'html') %>% cat(file = 'table.html')

# gt LaTeX table
msummary(models) %>% gtsave('table.tex')
```

## LaTeX

The `gt` LaTeX render engine is still immature. Until it improves, I strongly recommend that users turn to `kableExtra` to produce LaTeX tables. This packages offers robust functions that allow a lot of customization. A simple LaTeX table can be produced as follows:

```r
msummary(models, output = 'latex')
```

We can use functions from the `kableExtra` package to customize this table, with bold and colored cells, column spans, and more. For example, this code produces the table below.

```r
msummary(mod, output = 'latex',
         stars = TRUE,
         title = "This is a fancy table \\label{lab2}",
         notes = list('First note', 'Second note')) %>%
    add_header_above(c(" " = 1, "Group A" = 1, "Group B" = 2)) %>%
    row_spec(7, bold = TRUE, color = "white", background = "black") %>%
    column_spec(1, bold = TRUE, color = "red")
```

<img src="https://github.com/vincentarelbundock/modelsummary/blob/master/examples/kableExtra_latex.png" width="40%">

## `Rmarkdown`, `knitr`, and `Sweave`

You can use `modelsummary` to produce LaTeX tables and to create dynamic documents with `knitr` or `rmarkdown`. The correct `modelsummary` command to use depends on the output format.

* HTML documents: `modelsummary(models)` 
* PDF documents: `modelsummary(models, output = 'latex')` 

Note that the tables produced by `modelsummary` require the following LaTeX packages to compile: booktabs, array, float, colortbl, xcolor. These packages need to be included in the your header or preamble if you want documents to compile properly (see example files below).

Here are two minimal working examples of markdown files which can be converted to HTML or PDF using the `knitr` package. Just open one the `.Rmd` files in RStudio and click the "Knit" button:

* [markdown_to_pdf.Rmd](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_pdf.Rmd) / [markdown_to_pdf.pdf](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_pdf.pdf) 
* [markdown_to_html.Rmd](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_html.Rmd) / [markdown_to_html.html](https://github.com/vincentarelbundock/modelsummary/blob/master/examples/markdown_to_html.html) 

# Customizing your tables

## Uncertainty estimates: SE, t, p, CI

`modelsummary` prints an uncertainty estimate in parentheses below the corresponding coefficient estimate. The `statistic` argument must be a string which is equal to `conf.int` or to one of the columns produced by the `broom::tidy` function. When using `conf.int`, users can specify a confidence level with the `conf_level` argument.

```r
msummary(models, statistic = 'std.error')
msummary(models, statistic = 'p.value')
msummary(models, statistic = 'statistic')
msummary(models, statistic = 'conf.int', conf_level = .99)
```

Display the uncertainty estimate next to the coefficient instead of below it:

```r
msummary(models, statistic_vertical = FALSE)
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

You can also display several different uncertainty estimates below the coefficient estimates. For example,

```r
msummary(models, statistic = c('std.error', 'p.value', 'conf.int'))
```

Will produce something like this:

<img src="https://imgur.com/yNLr5Nt.png" width="30%">

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
msummary(models) %>% 
    tab_footnote(                                                                                                                                                                                  
        footnote = md("This is a **very** important model, so we are pointing it out in a column-specific footnote."),                        
        locations = cells_column_labels(columns = vars(`OLS 1`))) %>%  
    tab_footnote(                                                                                                                                                                               
        footnote = "This is the variable of interest.",                                                                                           
        locations = cells_body(columns = 1, rows = 3)) %>%                                                                                                                                      
    tab_footnote(                                                                                                                      
        footnote = "Most important model + most important variable = most important estimate.",
        locations = cells_body(columns = vars(`OLS 1`), rows = 3))
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
    tab_spanner(label = 'Literacy', columns = c('OLS 1', 'NBin 1')) %>%
    tab_spanner(label = 'Desertion', columns = c('OLS 2', 'NBin 2')) %>%
    tab_spanner(label = 'Clergy', columns = 'Logit 1')
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

## Extra tidy arguments (e.g., exponentiated coefficients)

Users can pass any additional argument they want to the `tidy` method which is used to extract estimates from a model. For example, in logitistic or Cox proportional hazard models, many users want to exponentiate coefficients to faciliate interpretation. The `tidy` functions supplied by the `broom` package allow users to set `exponentiate=TRUE` to achieve this. In `modelsummary`, users can use the same argument:

```r
mod_logit <- glm(am ~ mpg, data = mtcars, family = binomial)
msummary(mod_logit, exponentiate = TRUE)
```

Any argument supported by `tidy` is thus supported by `modelsummary`.

Warning: at the moment (2020-05-05), `broom::tidy` still reports `std.error` on the original scale. See this [discussion on the `broom` GitHub page.](https://github.com/tidymodels/broom/issues/422)

## Colors and styles

The power of the `gt` package makes `modelsummary` tables endlessly customizable. For instance, we can color columns and cells, and present values in bold or italics:

```r
msummary(models) %>%
    tab_style(style = cell_fill(color = "lightcyan"), 
			  locations = cells_body(columns = vars(`OLS 1`))) %>% 
    tab_style(style = cell_fill(color = "#F9E3D6"),
              locations = cells_data(columns = vars(`NBin 2`), rows = 2:6)) %>%
    tab_style(style = cell_text(weight = "bold"), 
		      locations = cells_body(columns = vars(`OLS 1`))) %>%
	tab_style(style = cell_text(style = "italic"), 
              locations = cells_data(columns = vars(`NBin 2`), rows = 2:6))
```

<img src="https://i.imgur.com/1u9hgm2.png" width="50%">

## Fancy text with markdown: bold, italics, etc.

Thanks to `gt`, `modelsummary` accepts markdown indications for emphasis and more:

```r
msummary(models, 
   title = md('This is a **bolded series of words.**'),
   notes = list(md('And an *emphasized note*.')))
```

## Font size

This will produce a table with extra large variable names.

```r
msummary(models) %>%
    tab_style(style = cell_text(size = 'x-large'),
              locations = cells_body(columns = 1)) 
```

Note that `gt`'s `tab_style` function is more developed for HTML output than for RTF or LaTeX, so some styling options may not be availble yet. The `gt` package is under heavy development, so feel free to file an issue on github if you have a special request, and stay tuned for more!

## Add rows manually

Use the `add_rows` argument to add rows manually to the bottom of the table.

```r
row1 <- c('Custom row 1', 'a', 'b', 'c', 'd', 'e')
row2 <- c('Custom row 2', 5:1)
msummary(models, add_rows = list(row1, row2))
```

Use the `add_rows` argument to specify where the custom rows should be displayed in the bottom panel. For example, this prints custom rows after the coefficients, but at first position in the goodness of fit measures:

```r
msummary(models, add_rows = list(row1, row2), add_rows_location = 0)
```

This prints custom rows after the 2nd GOF statistic:

```r
msummary(models, add_rows = list(row1, row2), add_rows_location = 2)
```

## Images

Insert images in your tables using the `gt::text_transform` and `gt::local_image` functions.

```r
msummary(models) %>%
    text_transform(
        locations = cells_body(columns = 1, rows = 1),
        fn = function(x) {web_image(url = "https://raw.githubusercontent.com/vincentarelbundock/modelsummary/master/examples/squirrel.png", height = 120)}
    )
```

# A complex example

This is the code I used to generate the "complex" table posted at the top of this README.

```r
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
        locations = cells_body(rows =5)) %>%                                                                                                                                      
    tab_footnote(                                                                                                                      
        footnote = "Most important model + most important variable = most important estimate.",
        locations = cells_body(columns = vars(`OLS 1`), rows = 5)) %>%   
    # color and bold
    tab_style(style = cell_text(color = "red", weight = "bold"),
              locations = cells_body(columns = vars(`OLS 1`), rows = 5))
```

# Other useful features


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

