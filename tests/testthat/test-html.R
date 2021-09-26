test_that("raw html output", {
    skip_on_os("windows")
    mod <- lm(hp ~ mpg, data = mtcars)
    expect_snapshot(modelsummary(mod, output = "html", gof_omit = ".*"))
})
