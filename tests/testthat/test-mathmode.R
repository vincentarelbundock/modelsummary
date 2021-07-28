mod <- list(
    lm(mpg ~ hp, mtcars),
    lm(mpg ~ hp + drat, mtcars))


test_that("known output", {
    expect_known_output(modelsummary(mod, align = "lS", output = "latex"),
                        file = "known_output/siunitx1.tex",
                        print = TRUE,
                        update = FALSE)
    expect_known_output(modelsummary(mod, align = "lS", output = "html"),
                        file = "known_output/mathmode_html_1.html",
                        print = TRUE,
                        update = FALSE)
})


test_that("supported outputs", {
    expect_error(modelsummary(mod, output = "latex", align = "lSS"), NA)
    expect_error(modelsummary(mod, output = "html", align = "lSS"), NA)
    expect_error(modelsummary(mod, output = "kableExtra", align = "lSS"), NA)
    expect_error(modelsummary(mod, output = "gt", align = "lSS"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "huxtable", align = "lSS"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "flextable", align = "lSS"), regexp = "only supported")
    expect_error(modelsummary(mod, output = "markdown", align = "lSS"), regexp = "only supported")
})


test_that("manual escape and math mode", {
    expect_error(modelsummary(mod, output = "latex", align = "lSS", escape = FALSE), NA)
    expect_error(modelsummary(mod, output = "latex", align = "lSS", escape = TRUE), regexp = "Cannot use")
    expect_error(modelsummary(mod, output = "html", align = "lSS", escape = FALSE), NA)
    expect_error(modelsummary(mod, output = "html", align = "lSS", escape = TRUE), NA)
    expect_error(modelsummary(mod, output = "kableExtra", align = "lSS", escape = FALSE), NA)
    expect_error(modelsummary(mod, output = "kableExtra", align = "lSS", escape = TRUE), NA)
})
