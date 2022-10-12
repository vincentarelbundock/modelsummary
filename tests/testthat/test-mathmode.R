mod <- list(
    lm(mpg ~ hp, mtcars),
    lm(mpg ~ hp + drat, mtcars))

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
