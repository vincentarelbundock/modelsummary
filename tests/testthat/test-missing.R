skip_if_not_installed("lme4")
requiet("lme4")

test_that('random effects variance components do not have standard errors and produce "empty"', {
    mod <- lme4::lmer(mpg ~ hp + (1 | gear), mtcars)
    tab <- modelsummary(mod, output = "data.frame")
    known <- c("(Intercept)", "(Intercept)", "hp", "hp", "SD (Intercept)", "SD (Observations)", "Num.Obs.", "R2 Marg.", "R2 Cond.", "AIC", "BIC", "ICC", "RMSE")
    expect_equal(tab$term, known)
})
