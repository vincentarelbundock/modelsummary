data(trees)
mod <- lm(Girth ~ Height + Volume, data = trees)

test_that("options(modelsummary_get)", {
    options(modelsummary_get = "broom")
    tab1 <- get_gof(mod)
    expect_equal(ncol(tab1), 13)

    options(modelsummary_get = "easystats")
    tab2 <- get_gof(mod)
    expect_equal(ncol(tab2), 7)
    expect_true("rmse" %in% colnames(tab2))

    options(modelsummary_get = "all")
    tab3 <- get_gof(mod)
    expect_equal(ncol(tab3), 16)
    options(modelsummary_get = "broom")
})
