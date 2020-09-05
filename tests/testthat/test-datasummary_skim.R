library(modelsummary)
context('datasummary_skim')

test_that('datasummary_skim type', {

  tmp_num <- mtcars[, c('mpg', 'hp')]
  tmp_cat <- mtcars[, c('cyl', 'am')]
  tmp_cat$cyl <- as.factor(tmp_cat$cyl)
  tmp_cat$am <- as.logical(tmp_cat$am)

  expect_error(datasummary_skim(tmp_num, type = 'categorical'))
  expect_error(datasummary_skim(tmp_cat, type = 'categorical'), NA)
  expect_error(datasummary_skim(tmp_num, type = 'numeric'), NA)
  expect_error(datasummary_skim(tmp_cat, type = 'numeric'))

})
