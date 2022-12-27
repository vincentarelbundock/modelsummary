requiet("fixest")


test_that("(non-)matching models", {
    pkgload::load_all()
    panels <- list(
        "Panel A: MPG" = list(
            lm(mpg ~ hp, data = mtcars),
            lm(mpg ~ hp + factor(gear), data = mtcars)),
        "Panel B: Displacement" = list(
            lm(disp ~ hp, data = mtcars),
            lm(disp ~ hp + factor(gear), data = mtcars))
    )
    tab1 <- panelsummary(panels, gof_map = "nobs", output = "dataframe")
    expect_equal(colnames(tab1), c(" ", "(1)", "(2)"))

    panelsummary(panels, gof_map = "nobs")

    panels <- list(
        "Panel A: MPG" = list(
            "A" = lm(mpg ~ hp, data = mtcars),
            "B" = lm(mpg ~ hp + factor(gear), data = mtcars)),
        "Panel B: Displacement" = list(
            "A" = lm(disp ~ hp, data = mtcars),
            "C" = lm(disp ~ hp + factor(gear), data = mtcars))
    )
    tab2 <- panelsummary(panels, gof_map = "nobs", output = "dataframe")
    expect_equal(colnames(tab2), c(" ", "A", "B", "C"))
})


test_that("informative errors", {
    expect_error(panelsummary(panels, shape = term ~ model), regexp = "shape.*not supported")
    expect_error(panelsummary(panels, group_map = list()), regexp = "group_map.*not supported")
})


test_that("stars note", {
    p <- suppressWarnings(panelsummary(panels, output = "markdown", stars = TRUE))
    expect_true(any(grepl("Note", p)))
})


test_that("output formats: no validity", {
    p <- panelsummary(panels, output = "gt")
    expect_s3_class(p, "gt_tbl")
    p <- panelsummary(panels, output = "latex")
    expect_s3_class(p, "knitr_kable")
})