context('add_rows')

library(modelsummary)

test_that("datasummary add_columns", {

  ac <- tibble::tribble(~first, ~last,
    'blah', 2,
    'junk', 4)
  attr(ac, 'position') <- c(1, NA)

  tab <- datasummary(mpg + hp ~ mean + sd,
    data = mtcars,
    add_columns = ac,
    fmt = '%.2f',
    output = 'dataframe')

  truth <- structure(list(first = c("blah", "junk"), ` ` = c("mpg", "hp"
  ), mean = c("20.09", "146.69"), sd = c("6.03", "68.56"), last = c("2.00",
    "4.00")), row.names = c(NA, -2L), class = "data.frame", align = "lrrrr", output_format = "dataframe")

  expect_identical(tab, truth)

})

test_that("too many rows in add_columns", {

  ac <- tibble::tribble(~first, ~last,
    'blah', 2,
    'junk', 4,
    'another', 5)

  expect_error(datasummary(mpg + hp ~ mean + sd, data = mtcars, add_columns = ac))

})
