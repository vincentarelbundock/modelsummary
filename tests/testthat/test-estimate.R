models <- list(
  lm(hp ~ mpg + drat, mtcars),
  lm(hp ~ mpg + drat, mtcars))

test_that("multiple estimates",{
  tab1 <- modelsummary(models, 
    output = "data.frame",
    gof_omit = ".*",
    stars = TRUE,
    statistic = NULL,
    estimate = c("{estimate} ({std.error}){stars}",
                 "{estimate} [{conf.low}, {conf.high}]"))
  tab2 <- modelsummary(models, 
    output = "data.frame",
    gof_omit = ".*",
    stars = FALSE,
    statistic = NULL,
    estimate = c("{estimate} ({std.error}){stars}",
                 "{estimate} [{conf.low}, {conf.high}]"))
  expect_true(all(tab1 == tab2))
})


test_that("error",{
  expect_error(
    modelsummary(models, 
      output = "data.frame",
      gof_omit = ".*",
      stars = TRUE,
      statistic = NULL,
      estimate = c("estimate",
                   "{estimate} ({std.error}){stars}",
                   "{estimate} [{conf.low}, {conf.high}]")))
})
