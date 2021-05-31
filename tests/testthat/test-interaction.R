# CRAN win-devel does not play well with fancy unicode x
skip_on_cran()

test_that(": in interactions become x", {
  mod <- lm(am ~ drat * mpg * vs, mtcars)
  expect_known_output(modelsummary(mod, "markdown"),
                      file="known_output/msummary_interaction.md",
                      print=TRUE,
                      update=FALSE)
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
