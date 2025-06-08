data(trees)
mod <- lm(Girth ~ Height + Volume, data = trees)

# options(modelsummary_get)
options(modelsummary_get = "broom")
tab1 <- get_gof(mod)
expect_equivalent(ncol(tab1), 13)

options(modelsummary_get = "easystats")
tab2 <- get_gof(mod)
expect_true(ncol(tab2) > 6)
expect_true("rmse" %in% colnames(tab2))

options(modelsummary_get = "all")
tab3 <- get_gof(mod)
expect_equivalent(ncol(tab3), 14)


# restore default
options(modelsummary_get = "easystats")
