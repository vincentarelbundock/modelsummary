test_that("lm f-stat accounts for vcov", {
    mod <- lm(mpg ~ hp + factor(cyl) + drat, data = mtcars)
    tab <- modelsummary(mod, output = "data.frame", vcov = list(NULL, "HC3"))
    f_iid <- sprintf("%.3f", lmtest::waldtest(mod)$F[2])
    f_hc3 <- sprintf("%.3f", lmtest::waldtest(mod, vcov = sandwich::vcovHC)$F[2])
    expect_equal(tab[tab$term == "F", "Model 1"], f_iid)
    expect_equal(tab[tab$term == "F", "Model 2"], f_hc3)
    expect_false(f_iid == f_hc3)
})


test_that("glm f-stat accounts for vcov", {
    mod <- glm(am ~ hp + factor(cyl) + drat, data = mtcars, family = binomial)
    tab <- modelsummary(mod, output = "data.frame", vcov = list(NULL, "HC3"))
    f_iid <- sprintf("%.3f", lmtest::waldtest(mod)$F[2])
    f_hc3 <- sprintf("%.3f", lmtest::waldtest(mod, vcov = sandwich::vcovHC)$F[2])
    expect_equal(tab[tab$term == "F", "Model 1"], f_iid)
    expect_equal(tab[tab$term == "F", "Model 2"], f_hc3)
    expect_false(f_iid == f_hc3)
})
