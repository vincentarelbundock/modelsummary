
test_that('random effects variance components do not have standard errors and produce "empty" (\num{NA})', {
    library(lme4)
    mod <- lmer(mpg ~ hp + (1 | gear), mtcars)
    expect_known_output(
        modelsummary(mod, output = "latex"),
        file = "known_output/na_1.tex",
        print = TRUE,
        update = TRUE)
    ## modelsummary(mod, statistic = "conf.int", output = "latex")
})
