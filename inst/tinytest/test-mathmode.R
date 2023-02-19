exit_file("snapshot")
mod <- list(
    lm(mpg ~ hp, mtcars),
    lm(mpg ~ hp + drat, mtcars))

# d-column: known output
    expect_snapshot(modelsummary(mod, align = "ldd", output = "latex"))



# HTML global options
    expect_snapshot(modelsummary(mod, output = "html"))
    options("modelsummary_format_numeric_html" = "dollars")
    expect_snapshot(modelsummary(mod, output = "html"))
    options("modelsummary_format_numeric_html" = "anything else")
    expect_snapshot(modelsummary(mod, output = "html"))
    options("modelsummary_format_numeric_html" = NULL)



# LaTeX global options
    expect_snapshot(modelsummary(mod, output = "latex"))
    options("modelsummary_format_numeric_latex" = "dollars")
    expect_snapshot(modelsummary(mod, output = "latex"))
    options("modelsummary_format_numeric_latex" = "anything else")
    expect_snapshot(modelsummary(mod, output = "latex"))
    options("modelsummary_format_numeric_latex" = NULL)

