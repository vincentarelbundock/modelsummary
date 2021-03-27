library(gamlss)
data(abdom)
mod <- list(
  gamlss(y ~ pb(x), sigma.fo = ~ pb(x),
         family = BCT, data = abdom, method = mixed(1, 20)),
  gamlss(y ~ x, sigma.fo = ~ pb(x),
         family = BCT, data = abdom, method = mixed(1, 20)))


test_that("grouped coefficients produce data.frames", {

    tab <- modelsummary(mod, "data.frame", group = term + component ~ model)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "term", "group", "statistic", "Model 1", "Model 2"))

    tab <- modelsummary(mod, "data.frame", group = component + term ~ model)
    expect_s3_class(tab, "data.frame")
    expect_equal(colnames(tab),
                 c("part", "group", "term", "statistic", "Model 1", "Model 2"))

    tab <- modelsummary(mod, "data.frame", group = term ~ model + component)
    expect_s3_class(tab, "data.frame")

    tab <- modelsummary(mod, "data.frame", group = term ~ component + model)
    expect_s3_class(tab, "data.frame")

    tab <- modelsummary(mod, "data.frame", group = term + model ~ component)
    expect_s3_class(tab, "data.frame")

    tab <- modelsummary(mod, "data.frame", group = model + term ~ component)
    expect_s3_class(tab, "data.frame")

})
