# results is the same, but order of `str()` components is swapped
skip_if(getRversion() < '4.0.0')

test_that("datasummary add_columns", {

  ac <- read.csv(text = 
"first,last
blah,2
junk,4")
  attr(ac, 'position') <- c(1, NA)

  tab <- datasummary(mpg + hp ~ mean + sd,
    data = mtcars,
    add_columns = ac,
    fmt = '%.2f',
    output = 'dataframe')
  expect_known_output(dput(tab),
                      file = "known_output/add_columns_1.R",
                      update = TRUE,
                      print = TRUE)
})

test_that("too many rows in add_columns", {


  ac <- read.csv(text =
    "first,last
     blah,2
     junk,4
     another,5")

  expect_error(datasummary(mpg + hp ~ mean + sd, data = mtcars, add_columns = ac))

})
