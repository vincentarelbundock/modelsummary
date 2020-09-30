context("modelsummary_list")

mod <- lm(mpg ~ hp + drat + vs, mtcars)

test_that("tidiers", {
  ml <- list(glance = glance(mod), tidy = tidy(mod))
  class(ml) <- "modelsummary_list"
  gl <- glance(ml)
  ti <- tidy(ml)
  expect_is(gl, "data.frame")
  expect_is(ti, "data.frame")
  expect_equal(dim(ti), c(4, 5))
  expect_equal(dim(gl), c(1, 12))
})

test_that("tidiers empty", {
  ml <- list(tidy = tidy(mod))
  class(ml) <- "modelsummary_list"
  gl <- glance(ml)
  expect_is(gl, "data.frame")
  expect_equal(dim(gl), c(1, 0))

  ml <- list(glance = glance(mod))
  class(ml) <- "modelsummary_list"
  expect_error(tidy(ml))
})
