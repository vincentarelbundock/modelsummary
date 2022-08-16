test_that("raw html output", {
    skip_on_os("windows")
    mod <- lm(hp ~ mpg, data = mtcars)
    expect_snapshot(modelsummary(mod, output = "html", gof_omit = ".*"))
})


test_that("PR 538", {
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    tab <- msummary(mod, stars = TRUE, output = "html")
    expect_true(grepl("\\&lt", tab))
})
