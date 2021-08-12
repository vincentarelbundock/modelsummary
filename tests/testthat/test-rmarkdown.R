# Not sure why this doesn't pass on Github Actions
skip_on_ci()

# solaris failure and complaints about pandoc in "Writing R Extensions" ("annoyingly so")
skip_on_cran() 


dangerous_document <- '
---
title: test
---

Table \\ref{tab:simple}

```{r}
library(modelsummary)
```

```{r}
datasummary_skim(mtcars)
```

```{r}
datasummary_balance(~vs, data = mtcars)
```

```{r}
datasummary_correlation(mtcars)
```

```{r simple}
dat <- mtcars
colnames(dat)[1] <- "under_score"
mod <- lm(hp ~ under_score + drat, dat)
modelsummary(mod, title = "Some caption content.")
```

```{r siunitx}
if (knitr::is_latex_output()) {
  modelsummary(mod, title = "Some caption content.", align = "ld")
}
```
'

test_that("Compile dangerous Rmarkdown to PDF and HTML", {
    rmd_file <- tempfile(fileext = ".Rmd")
    pdf_file <- gsub("\\.Rmd$", ".pdf", rmd_file )
    html_file <- gsub("\\.Rmd$", ".html", rmd_file )
    cat(dangerous_document, file = rmd_file)
    expect_error(rmarkdown::render(rmd_file,
                                   output_format = "pdf_document",
                                   output_file = pdf_file,
                                   quiet = TRUE),
                 NA)
    expect_error(rmarkdown::render(rmd_file,
                                   output_format = "html_document",
                                   output_file = html_file,
                                   quiet = TRUE),
                 NA)
})
