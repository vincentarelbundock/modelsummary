mod <- list(
    lm(mpg ~ hp, mtcars),
    lm(mpg ~ hp + drat, mtcars))


test_that("S-column: known output", {
    expect_known_output(modelsummary(mod, align = "lSS", output = "latex"),
                        file = "known_output/mathmode_1.tex",
                        print = TRUE,
                        update = FALSE)
})


test_that("S-column: supported outputs", {
    expect_error(modelsummary(mod, output = "latex", align = "lSS"), NA)
    expect_error(modelsummary(mod, output = "latex_tabular", align = "lSS"), NA)
    expect_error(modelsummary(mod, output = "html", align = "lSS"), "only supported")
    expect_error(modelsummary(mod, output = "kableExtra", align = "lSS"), "only supported")
    expect_error(modelsummary(mod, output = "gt", align = "lSS"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "huxtable", align = "lSS"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "flextable", align = "lSS"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "markdown", align = "lSS"), regexp = "only supported")
})


test_that("S-column: manual escape", {
    expect_error(modelsummary(mod, output = "latex", align = "lSS", escape = FALSE), NA)
    expect_error(modelsummary(mod, output = "latex", align = "lSS", escape = TRUE), regexp = "Cannot use")
    expect_error(modelsummary(mod, output = "latex_tabular", align = "lSS", escape = FALSE), NA)
    expect_error(modelsummary(mod, output = "latex_tabular", align = "lSS", escape = TRUE), regexp = "Cannot use")
})
