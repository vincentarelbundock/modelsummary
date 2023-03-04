source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

# : in interactions become x
mod <- lm(am ~ drat * mpg * vs, mtcars)
expect_snapshot_print(
  modelsummary(mod, "markdown"),
  "interaction-markdown")

# fixest i() becomes =
requiet("fixest")
mod <- suppressMessages(feols(Ozone ~ Solar.R + i(Month), airquality))
expect_snapshot_print(
  modelsummary(mod, "markdown", gof_map = list()),
  "interaction-markdown_no_gof")

# conditional conversion of : to x
mod <- lm(am ~ drat:mpg, mtcars)
tab <- modelsummary(
  mod,
  output = "dataframe",
  coef_rename = c("DRAT" = "drat"))
expect_true("drat:mpg" %in% tab$term)

mod <- lm(mpg ~ disp, mtcars)
tab <- modelsummary(
  mod,
  output = "dataframe",
  coef_rename = c("disp" = "a:b"))
expect_true("a:b" %in% tab$term)