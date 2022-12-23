requiet("fixest")
panels <- list(
    "Panel A: MPG" = list(
        feols(mpg ~  cyl | gear + carb, cluster = ~hp, data = mtcars),
        feols(mpg ~  cyl | gear + carb + am, cluster = ~hp, data = mtcars)),
    "Panel B: Displacement" = list(
        feols(disp ~ cyl | gear + carb, cluster = ~hp, data = mtcars),
        feols(disp ~ cyl | gear + carb + am, cluster = ~hp, data = mtcars))
)


test_that("informative errors", {
    expect_error(panelsummary(panels, shape = term ~ model), regexp = "shape.*not supported")
    expect_error(panelsummary(panels, group_map = list()), regexp = "group_map.*not supported")
})


test_that("output formats: no validity", {
    p <- panelsummary(panels, output = "gt")
    expect_s3_class(p, "gt_tbl")
    p <- panelsummary(panels, output = "latex")
    expect_s3_class(p, "knitr_kable")
})