test_that("markdown d-column snapshot", {
    mod <- lm(mpg ~ I(hp / 1000) + am + vs + factor(cyl), mtcars)
    expect_snapshot(
        modelsummary(mod, "markdown", align = "ld")
    )
})