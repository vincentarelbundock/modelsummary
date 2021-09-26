mod <- list(
    lm(mpg ~ hp, mtcars),
    lm(mpg ~ hp + drat, mtcars))


test_that("d-column: supported outputs", {
    expect_error(modelsummary(mod, output = "latex", align = "ldd"), NA)
    expect_error(modelsummary(mod, output = "latex_tabular", align = "ldd"), NA)
    expect_error(modelsummary(mod, output = "html", align = "ldd"), "only supported")
    expect_error(modelsummary(mod, output = "kableExtra", align = "ldd"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "gt", align = "ldd"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "huxtable", align = "ldd"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "flextable", align = "ldd"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "markdown", align = "ldd"), regexp = "only supported")
})


test_that("d-column: unsupported arguments", {
    expect_error(modelsummary(mod, align = "ldd", output = "latex", stars = TRUE),
                 regexp = "align.*supported")
    expect_error(modelsummary(mod, align = "ldd", output = "latex", statistic = "conf.int"),
                 regexp = "align.*supported")
    expect_error(modelsummary(mod, align = "ldd", output = "latex", estimate = "conf.int"),
                 regexp = "align.*supported")
    expect_error(modelsummary(mod, align = "ldd", output = "latex", statistic = "p = {p.value}"),
                 regexp = "align.*supported")
    expect_error(modelsummary(mod, align = "ldd", output = "latex", estimate = "p = {p.value}"),
                 regexp = "align.*supported")
})


test_that("d-column: known output", {
    expect_snapshot(modelsummary(mod, align = "ldd", output = "latex"))
})



test_that("HTML global options", {
    skip_on_os("windows")
    expect_snapshot(modelsummary(mod, output = "html"))
    options("modelsummary_format_numeric_html" = "dollars")
    expect_snapshot(modelsummary(mod, output = "html"))
    options("modelsummary_format_numeric_html" = "anything else")
    expect_snapshot(modelsummary(mod, output = "html"))
    options("modelsummary_format_numeric_html" = NULL)
})


test_that("LaTeX global options", {
    expect_snapshot(modelsummary(mod, output = "latex"))
    options("modelsummary_format_numeric_latex" = "dollars")
    expect_snapshot(modelsummary(mod, output = "latex"))
    options("modelsummary_format_numeric_latex" = "anything else")
    expect_snapshot(modelsummary(mod, output = "latex"))
    options("modelsummary_format_numeric_latex" = NULL)
})
