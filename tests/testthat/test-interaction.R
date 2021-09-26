# CRAN win-devel does not play well with fancy unicode x
skip_on_cran()
skip_if_not_installed("fixest")
requiet("fixest")

test_that(": in interactions become x", {
  mod <- lm(am ~ drat * mpg * vs, mtcars)
  expect_snapshot_output(modelsummary(mod, "markdown"))
})

test_that("fixest i() becomes =", {
  testthat::skip_if_not_installed("fixest")
  mod <- suppressMessages(feols(Ozone ~ Solar.R + i(Month), airquality))
  expect_snapshot_output(modelsummary(mod, "markdown", gof_map = list()))
})

test_that("conditional conversion of : to x", {
    mod <- lm(am ~ drat : mpg, mtcars)
    tab <- modelsummary(
        mod,
        output = "dataframe",
        coef_rename = c("DRAT" = "drat"))
    expect_true("drat:mpg" %in% tab$term)

    mod <- lm(mpg ~ disp, mtcars)
    tab <- modelsummary(
        mod,
        output = "dataframe",
        coef_rename = c("disp" = "a:b"))
    expect_true("a:b" %in% tab$term)
})
