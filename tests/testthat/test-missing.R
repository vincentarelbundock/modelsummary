skip_if_not_installed("lme4")
requiet("lme4")

test_that('random effects variance components do not have standard errors and produce "empty"', {
    mod <- lme4::lmer(mpg ~ hp + (1 | gear), mtcars)
    expect_snapshot(cat(modelsummary(mod, output = "latex")))
})
