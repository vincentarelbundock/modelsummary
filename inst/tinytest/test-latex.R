source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

models <- list(
  lm(hp ~ mpg, mtcars),
  lm(hp ~ mpg + drat, mtcars))

# latex threeparttable argument
options(modelsummary_factory_latex = "kableExtra")
tab1 <- modelsummary(models, output = "latex", stars = TRUE)
tab2 <- modelsummary(models, output = "latex", threeparttable = TRUE, stars = TRUE)
expect_false(grepl("threeparttable", tab1))
expect_true(grepl("threeparttable", tab2))
expect_equivalent(sum(grepl("threeparttable", strsplit(tab2, "\n")[[1]])), 2)

## kableExtra::footnote has a bug with multiple footnotes and threeparttable, so we combine notes.
ft <- "Here is a very very very very very very very very very very very very very very very very very long footnote"
tab3 <- modelsummary(models,
  output = "latex",
  title = "Regression output",
  notes = ft,
  stars = TRUE,
  threeparttable = TRUE)
expect_equivalent(sum(grepl("threeparttable", strsplit(tab3, "\n")[[1]])), 2)
options(modelsummary_factory_latex = NULL)

# output = latex_tabular
expect_snapshot_print(
  modelsummary(models, output = "latex_tabular"),
  "latex-tabular")

# Issue #560: circum escape
requiet("fixest")
base = iris
names(base) = c("y", paste0("x", 1:3), "fe1")
base$fe2 = rep(letters[1:5], 30)
base$fe3 = rep(letters[1:5], 30)
est_comb = feols(y ~ x1 | fe1 + fe2 + fe3, data = base, cluster = "fe1^fe2")
tab <- modelsummary(est_comb, output = "latex")
expect_true(grepl("circum\\{\\}", tab))
