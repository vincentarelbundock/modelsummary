source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

models <- list()
models[["OLS 1"]] <- lm(hp ~ mpg + wt, mtcars)
models[["Poisson 1"]] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[["OLS 2"]] <- lm(vs ~ hp + wt, mtcars)
models[["Logit 1"]] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[["Logit 2"]] <- glm(am ~ hp + disp, mtcars, family = binomial())

# knitr::kable_latex ignores bad arguments passed through ...
tab <- modelsummary(models, output = "latex", badarg = TRUE)
expect_inherits(tab, "knitr_kable")

# output="html" returns raw html
tab <- modelsummary(models, output = "html")
expect_identical(class(tab), c("modelsummary_string", "kableExtra", "knitr_kable"))

# kable markdown: complex table
cm <- c(
  "hp" = "Horsepower",
  "mpg" = "Miles/Gallon",
  "wt" = "Weight",
  "drat" = "Rear axle ratio",
  "disp" = "Displacement",
  "(Intercept)" = "Constant")

expect_snapshot_print(
  modelsummary(
    models,
    coef_map = cm,
    stars = TRUE,
    gof_omit = "Statistics|^p$|Deviance|Resid|Sigma|Log.Lik|^DF$",
    title = "Summarizing 5 statistical models using the `modelsummary` package for `R`.",
    notes = c(
      "First custom note to contain text.",
      "Second custom note with different content."),
    output = "markdown"),
    "kableExtra-markdown_complex")

# kable markdown: rouding + custom stars
expect_snapshot_print(
  modelsummary(
    models,
    stars = c("+" = .1, "*" = .01),
    fmt = "%.8f",
    output = "markdown"),
    "kableExtra-markdown_fmt")

# Issue #548: titles escaped in kableExtra
mod <- lm(mpg ~ hp, mtcars)
tab <- modelsummary(mod, "latex", title = "blah_cyl", escape = TRUE)
expect_true(grepl("blah\\\\_cyl", tab))
tab <- modelsummary(mod, "latex", title = "blah_cyl", escape = FALSE)
expect_false(grepl("blah\\\\_cyl", tab))