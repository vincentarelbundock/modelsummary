requiet("lme4")

test_that("performance metrics", {
    N <- 1e4
    dat <- data.frame(
      x = rnorm(N),
      y = rnorm(N),
      k = factor(sample(1:50, N, replace = TRUE)),
      m = factor(sample(1:1000, N, replace = TRUE)))
    mod <- lmer(y ~ x + (1 | k) + (1 | m), data = dat)

    expect_warning(modelsummary(mod, group = term + group ~ model))
    tab1 <- modelsummary(mod, output = "data.frame", group = term + group ~ model)
    tab2 <- modelsummary(mod, output = "data.frame", group = term + group ~ model, metrics = c("RMSE", "R2"))
    expect_true("RMSE" %in% tab1$term)
    expect_false("R2" %in% tab1$term)
    expect_true(all(c("RMSE", "R2 Marg.", "R2 Cond.") %in% tab2$term))
})
