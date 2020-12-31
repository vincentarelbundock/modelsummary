models <- list(
  lm(hp ~ mpg, mtcars),
  lm(hp ~ mpg + drat, mtcars))


test_that("stars_note < are protected by $ in latex", {
  tab <- modelsummary(models, stars=TRUE, output="latex")
  expect_true(grepl("p $<$ 0.1", tab, fixed=TRUE))
})


test_that("model names protected by multicolumn with dcolumn",{
  tab <- modelsummary(
    models, 
    stars = TRUE, 
    output = "latex",
    align = c("l", "D{.}{.}{-1}", "D{.}{.}{-1}"))
  expect_true(grepl("multicolumn....c..Model", tab))
})
