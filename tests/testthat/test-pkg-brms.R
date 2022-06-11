skip_on_cran()

test_that("brms: diagnostics and tests", {
    requiet("brms")
    mod <- insight::download_model("brms_1")
    expect_error(modelsummary(mod, "data.frame", statistic = "conf.int"), NA)
    expect_error(modelsummary(mod, "data.frame", statistic = "rope"), regexp = "available")
    expect_error(modelsummary(mod, "data.frame", diagnostic = "ESS", statistic = "ess"), NA)
})
