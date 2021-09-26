test_that("renamed factory global options", {
    options(modelsummary_default = "markdown")
    mod <- lm(mpg ~ hp, mtcars)
    expect_warning(tab <- modelsummary(mod), regexp = "global option is deprecated")
    options(modelsummary_default = NULL)
})
