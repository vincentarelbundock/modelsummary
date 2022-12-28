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
  expect_snapshot(dput(tab))
})

test_that("too many rows in add_columns", {

  ac <- read.csv(text =
    "first,last
     blah,2
     junk,4
     another,5")

  expect_error(datasummary(mpg + hp ~ mean + sd, data = mtcars, add_columns = ac))

})


test_that("add_columns support in modelsummary.", {
  mod <- lm(mpg ~ hp, mtcars)
  ac <- data.frame(X = letters[1:4])
  attr(ac, "position") <- 2
  tab <- modelsummary(mod, output = "dataframe", gof_map = NA, add_columns = ac)
  expect_equal("X", colnames(tab)[2])
  expect_equal(ncol(tab), 5)
})
