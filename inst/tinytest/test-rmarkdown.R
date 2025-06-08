# solaris failure and complaints about pandoc in "Writing R Extensions" ("annoyingly so")
# pkgdown failure on github actions
# exit_file("tinytable: escape latex")
source("helpers.R")
if (ON_CI) exit_file("CI")
# exit_file("tinytable + rmarkdown")

dangerous_document <- '
---
title: test
output: %s
---

Table \\ref{tab-simple}

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

```{r siunitx, eval = FALSE}
if (knitr::is_latex_output()) {
  modelsummary(mod, title = "Some caption content.", align = "ld")
}
```
'
rmd_file <- paste0(random_string(), ".Rmd")

# Rmarkdown to pdf_document
## not sure why PDF compilation doesn't work on Github actions
cat(sprintf(dangerous_document, "pdf_document"), file = rmd_file)
pdf_file <- gsub("\\.Rmd$", ".pdf", rmd_file)
suppressWarnings(rmarkdown::render(
  rmd_file,
  output_file = pdf_file,
  quiet = TRUE
))


# # Rmarkdown to html_document
# cat(sprintf(dangerous_document, "html_document"), file = rmd_file)
# html_file <- gsub("\\.Rmd$", ".html", rmd_file )
# rmarkdown::render(rmd_file, output_file = html_file, quiet = TRUE)

# # Rmarkdown to word_document
# cat(sprintf(dangerous_document, "word_document"), file = rmd_file)
# docx_file <- gsub("\\.Rmd$", ".docx", rmd_file )
# rmarkdown::render(rmd_file, output_file = docx_file, quiet = TRUE)

# # does not even work interactively
# # # Rmarkdown to bookdown::word_document2
# # cat(sprintf(dangerous_document, "bookdown::word_document2"), file = rmd_file)
# # docx_file <- gsub("\\.Rmd$", ".docx", rmd_file )
# # rmarkdown::render(rmd_file, output_file = docx_file, quiet = TRUE)

unlink(rmd_file)
# unlink(pdf_file)
# unlink(docx_file)
# unlink(html_file)
