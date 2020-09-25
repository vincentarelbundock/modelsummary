# TODO: add_columns

library(modelsummary)

context("datasummyary_df")

test_that("no error", {

  tmp <- head(mtcars)

  ar <- tail(mtcars, 1)
  ac <- data.frame(new = 1:6)

  expect_error(
    datasummary_df(
      data=tmp,
      output="default",
      fmt="%.0f",
      align=strrep("r", ncol(tmp)),
      hrule=3,
      title="blah blah title",
      notes=c("first note", "second note"),
      add_rows=ar),
    NA
  )

})
