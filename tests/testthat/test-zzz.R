test_that("glue with no statistic", {
    # first time lmer raises a warn_once
    requiet("lme4")
    mod <- lmer(mpg ~ hp + (1 | cyl), data = mtcars)
    tab <- modelsummary(mod,
        output = "data.frame",
        gof_map = NA,
        statistic = c(
            "t = {statistic}",
            "p = {p.value}"))
    expect_equal(nrow(tab), 8)
})
