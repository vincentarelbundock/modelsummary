context("unsupported models")

test_that('Characters raise error', {
  mod <- list()
  mod[[1]] <- lm(hp ~ mpg, mtcars)
  mod[[2]] <- 'ljaksdf'
  expect_error(msummary(mod))
  expect_error(msummary(mod[[2]]))
})

test_that('Numerics raise error', {
  mod <- list()
  mod[[1]] <- lm(hp ~ mpg, mtcars)
  mod[[2]] <- 1:30
  expect_warning(expect_error(msummary(mod)))
  expect_warning(expect_error(msummary(mod[[2]])))
})

test_that('NULL raises error', {
  mod <- lm(hp ~ mpg, mtcars)
  mod <- list(mod, NULL)
  expect_error(msummary(mod))
  expect_error(msummary(mod[[2]]))
})
