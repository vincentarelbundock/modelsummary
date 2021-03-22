test_that("automatic standard errors labelling", {
    mod <- lm(hp ~ mpg, mtcars)

    vcov <- list("iid", "robust", "stata", ~cyl, ~vs:am)
    tab <- modelsummary(mod, vcov = vcov, output = "dataframe")
    expect_true("Std. Errors" %in% tab[[2]])
    expect_true("IID" %in% tab[[4]])
    expect_true("Robust" %in% tab[[5]])
    expect_true("Stata" %in% tab[[6]])
    expect_true("C: cyl" %in% tab[[7]])
    expect_true("C: vs:am" %in% tab[[8]])

    vcov <- list("iid", "robust")
    tab <- modelsummary(mod, vcov = vcov, output = "dataframe")
    expect_true("Std. Errors" %in% tab[[2]])
    expect_true("IID" %in% tab[[4]])
    expect_true("Robust" %in% tab[[5]])
})
