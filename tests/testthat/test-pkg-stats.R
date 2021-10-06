test_that("stats::lm F tests conform to `vcov` argument", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    res <- modelsummary(mod, "data.frame", vcov = c("HC1", "stata", "HC2", "classical"))
    expect_true(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 2"])
    expect_false(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 3"])
    expect_false(res[res$term == "F", "Model 1"] == res[res$term == "F", "Model 4"])
    expect_false(res[res$term == "F", "Model 2"] == res[res$term == "F", "Model 3"])
    expect_false(res[res$term == "F", "Model 2"] == res[res$term == "F", "Model 4"])
})
