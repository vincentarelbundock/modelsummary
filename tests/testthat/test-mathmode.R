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
    expect_known_output(modelsummary(mod, align = "ldd", output = "latex"),
                        file = "known_output/mathmode_1.tex",
                        print = TRUE,
                        update = TRUE)
})



test_that("HTML global options", {
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/mathmode_2.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_format_numeric_html" = "dollars")
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/mathmode_3.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_format_numeric_html" = "anything else")
    expect_known_output(modelsummary(mod, output = "html"),
                        file = "known_output/mathmode_4.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_format_numeric_html" = NULL)
})



test_that("LaTeX global options", {
    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/mathmode_5.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_format_numeric_latex" = "dollars")
    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/mathmode_6.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_format_numeric_latex" = "anything else")
    expect_known_output(modelsummary(mod, output = "latex"),
                        file = "known_output/mathmode_7.tex",
                        print = TRUE,
                        update = TRUE)
    options("modelsummary_format_numeric_latex" = NULL)
})
