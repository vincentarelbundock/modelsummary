test_that("raw html output", {
    mod <- lm(hp ~ mpg, data = mtcars)
    expect_known_output(modelsummary(mod,
                                     output = "html",
                                     gof_omit = ".*"),
                        file = "known_output/html_1.tex",
                        print = TRUE,
                        update = FALSE)
})
