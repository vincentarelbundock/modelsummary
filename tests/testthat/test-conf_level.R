test_that("Issue 513", {
    # bad combination of arguments
    mod <- lm(mpg ~ hp, data = mtcars)
    expect_error(
        modelsummary(
            mod,
            output = "data.frame",
            statistic = "conf.int",
            conf_level = NULL),
        pattern = "Cannot display")

    # baseline
    tab <- modelsummary(
            mod,
            output = "data.frame",
            statistic = "conf.int",
            conf_level = .9)
    expect_true(any(grepl("\\[", tab[["Model 1"]])))

    # faster
    est <- get_estimates(mod, conf_level = NULL)
    expect_false("conf.low" %in% colnames(est))
})
