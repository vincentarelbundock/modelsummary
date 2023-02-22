source("helpers.R")
requiet("MASS")

# Issue #518
# lmtest::coeftest does not return values for intercepts
mod <- polr(factor(gear) ~ mpg, data = mtcars)
tab <- suppressMessages(suppressWarnings(modelsummary(
    mod,
    output = "dataframe",
    estimate = "std.error",
    statistic = NULL,
    gof_map = "none",
    vcov = list(NULL, ~cyl))))
expect_true(all(tab[["(1)"]] != tab[["(2)"]]))

