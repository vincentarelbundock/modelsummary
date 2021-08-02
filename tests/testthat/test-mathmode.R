mod <- list(
    lm(mpg ~ hp, mtcars),
    lm(mpg ~ hp + drat, mtcars))

## pkgload::load_all("~/repos/modelsummary")
## modelsummary(mod)

## tmp = modelsummary(mod)
## modelsummary(mod, "dataframe")
## modelsummary(mod, "html")

## devtools::install("~/repos/modelsummary")
## devtools::check("~/repos/modelsummary")
## devtools::document("~/repos/modelsummary")
## library(modelsummary)


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



test_that("S-column: known output", {
    expect_known_output(modelsummary(mod, align = "lSS", output = "latex"),
                        file = "known_output/mathmode_1.tex",
                        print = TRUE,
                        update = TRUE)
})



test_that("HTML global options", {
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/mathmode_2.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_math_html" = "dollars")
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/mathmode_3.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_math_html" = "anything else")
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/mathmode_4.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_math_html" = NULL)
})



test_that("LaTeX global options", {
    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/mathmode_5.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_math_latex" = "dollars")
    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/mathmode_6.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_math_latex" = "anything else")
    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/mathmode_7.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_math_latex" = NULL)
})
