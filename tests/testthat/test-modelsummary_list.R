mod <- lm(mpg ~ hp + drat + vs, mtcars)

# this is tricky because get_estimates doesn't always return predictable results depending on whether parameters, broom, or broom.mixed are loaded. this is a problem.

# test_that("tidiers", {
#   ml <- list(
#     glance = modelsummary::get_gof(mod), 
#     tidy = modelsummary::get_estimates(mod))
#   class(ml) <- "modelsummary_list"
#   gl <- generics::glance(ml)
#   ti <- generics::tidy(ml)
#   expect_is(gl, "data.frame")
#   expect_is(ti, "data.frame")
#   # # broom
#   # expect_equal(dim(ti), c(4, 5))
#   # expect_equal(dim(gl), c(1, 12))
#   # easystats
#   expect_equal(dim(ti), c(4, 6))
#   expect_equal(dim(gl), c(1, 9))
# })

test_that("tidiers empty", {
  ml <- list(tidy = modelsummary:::get_estimates(mod))
  class(ml) <- "modelsummary_list"
  gl <- generics::glance(ml)
  expect_is(gl, "data.frame")
  expect_equal(dim(gl), c(1, 0))
  ml <- list(glance = modelsummary:::get_gof(mod))
  class(ml) <- "modelsummary_list"
  expect_error(tidy(ml))
})
