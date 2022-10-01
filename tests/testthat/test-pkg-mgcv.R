requiet("mgcv")

test_that("Issue #558", {
    dat <- gamSim(1, n = 4000, dist = "normal", scale = 2, verbose = FALSE)
    mod <- gam(y ~ s(x0) + s(x1) + s(x2), data = dat)
    tab <- modelsummary(
        mod,
        output = "dataframe",
        coef_omit = "^s\\(",
        statistic = c(
            "std.error",
            "statistic",
            "p.value"),
        shape = term ~ model + statistic,
        gof_map = NA)
    expect_equal(tab[1, ncol(tab)], "<0.001")
})