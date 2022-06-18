requiet("lavaan")
requiet("parameters")

test_that("Issue #502", {
    skip_if_not_installed("parameters", minimum_version = "0.18.1.3")
    model <- 'ind60 =~ x1 + x2 + x3'
    fit <- cfa(model, data = PoliticalDemocracy)
    void <- capture.output({
     tab <- modelsummary(fit, output = "data.frame", standardize = TRUE)
    })
    expect_s3_class(tab, "data.frame")
    expect_true(nrow(tab) > 7)
    expect_true(ncol(tab) > 3)
})
