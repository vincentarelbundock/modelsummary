requiet("fixest")

panels <- list(
    "Panel A: MPG" = list(
        "A" = lm(mpg ~ hp, data = mtcars),
        "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
    "Panel B: Displacement" = list(
        "A" = lm(disp ~ hp, data = mtcars),
        "C" = lm(disp ~ hp + factor(gear), data = mtcars))
)


test_that("(non-)matching models", {
    panels <- list(
        "Panel A: MPG" = list(
            lm(mpg ~ hp, data = mtcars),
            lm(mpg ~ hp + factor(gear), data = mtcars)),
        "Panel B: Displacement" = list(
            lm(disp ~ hp, data = mtcars),
            lm(disp ~ hp + factor(gear), data = mtcars))
    )
    tab1 <- modelsummary(panels, gof_map = "nobs", output = "dataframe", shape = "rbind")
    expect_equal(colnames(tab1), c(" ", "(1)", "(2)"))

    panels <- list(
        "Panel A: MPG" = list(
            "A" = lm(mpg ~ hp, data = mtcars),
            "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
        "Panel B: Displacement" = list(
            "A" = lm(disp ~ hp, data = mtcars),
            "C" = lm(disp ~ hp + factor(gear), data = mtcars))
    )
    tab2 <- modelsummary(panels, gof_map = "nobs", output = "dataframe", shape = "rbind")
    expect_equal(colnames(tab2), c(" ", "A", "B", "C"))
})


test_that("stars note", {
    p <- suppressWarnings(modelsummary(panels, output = "markdown", stars = TRUE, shape = "rbind"))
    expect_true(any(grepl("Note", p)))
})


test_that("output formats: no validity", {
    p <- modelsummary(panels, output = "gt", shape = "rbind")
    expect_s3_class(p, "gt_tbl")
    p <- modelsummary(panels, output = "latex", shape = "rbind")
    expect_s3_class(p, "knitr_kable")
})
