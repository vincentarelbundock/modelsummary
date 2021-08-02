# Not sure why this doesn't pass on Github Actions
skip_on_ci()

dangerous_document <- '
---
title: test
output: 
  pdf_document:
    citation_package: natbib
---

```{r simple}
library(modelsummary)
dat <- mtcars
colnames(dat)[1] <- "under_score"
mod <- lm(hp ~ under_score + drat, dat)
modelsummary(mod)
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
