source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

# raw html output
mod <- lm(hp ~ mpg, data = mtcars)
expect_snapshot_print(
    modelsummary(mod, output = "html", gof_omit = ".*"),
    "html-gof_omit")

# PR 538
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
tab <- msummary(mod, stars = TRUE, output = "html")
expect_true(grepl("\\&lt", tab))