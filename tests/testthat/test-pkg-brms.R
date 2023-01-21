skip_on_cran()
requiet("brms")

test_that("brms: diagnostics and tests", {
    mod <- insight::download_model("brms_1")
    expect_error(modelsummary(mod, "data.frame", statistic = "conf.int"), NA)
    expect_error(modelsummary(mod, "data.frame", statistic = "rope"), regexp = "available")
    expect_error(modelsummary(mod, "data.frame", diagnostic = "ESS", statistic = "ess"), NA)
})


test_that("modelplot", {
    mod <- marginaleffects:::modelarchive_model("brms_numeric2")
    p <- modelplot(mod)
    expect_s3_class(p, "gg")
    p <- modelplot(mod, draw = FALSE)
    expect_equal(nrow(p), 3)
})