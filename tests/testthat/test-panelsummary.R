requiet("fixest")
panels <- list(
    "Panel A: MPG" = list(
        feols(mpg ~  cyl | gear + carb, data = mtcars),
        feols(mpg ~  cyl | gear + carb + am, data = mtcars)),
    "Panel B: Displacement" = list(
        feols(disp ~ cyl | gear + carb, data = mtcars),
        feols(disp ~ cyl | gear + carb + am, data = mtcars))
)


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



# panels <- list(
#     "Panel 1: MPG" = list(
#         "I" = lm(mpg ~  factor(cyl), data = mtcars),
#         "II" = lm(mpg ~  factor(cyl) + am, data = mtcars)),
#     "Panel 2: Displacement" = list(
#         "I" = lm(disp ~ factor(cyl), data = mtcars),
#         "II" = lm(disp ~ factor(cyl) + am, data = mtcars))
# )
# panelsummary(
#     panels,
#     title = "Four regression models in two panels.",
#     coef_rename = TRUE,
#     gof_map = c("nobs", "r.squared"))

# panels <- list(
#     list(
#         lm(mpg ~  factor(cyl), data = mtcars),
#         lm(mpg ~  factor(cyl) + am, data = mtcars)),
#     list(
#         lm(disp ~ factor(cyl), data = mtcars),
#         lm(disp ~ factor(cyl) + am, data = mtcars))
# )
# panelsummary(
#     panels,
#     title = "Four regression models in two panels.",
#     coef_rename = TRUE,
#     gof_map = c("nobs", "r.squared"))
