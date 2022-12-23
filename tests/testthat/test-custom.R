mod <- glm(am ~ mpg, mtcars, family = binomial)

###################
#  glance_custom  #
###################
test_that("glance_custom.glm", {
  glance_custom.glm <- function(x) {
    data.frame("test" = 1.54, "test2" = "lkkd", "test3" = as.integer(2), "test4" = TRUE)
  }
  # beware of testthat scoping issue
  assign("glance_custom.glm", glance_custom.glm, envir = .GlobalEnv)
  out <- modelsummary(mod, "data.frame")
  expect_equal(dim(out), c(14, 4))
  rm("glance_custom.glm", envir = .GlobalEnv)
})

test_that("glance_custom.glm preserve order", {
  glance_custom.glm <- function(x) {
    data.frame("test5" = 1.54, "test6" = "lkkd", "test3" = as.integer(2), "test4" = TRUE)
  }
  # beware of testthat scoping issue
  assign("glance_custom.glm", glance_custom.glm, envir = .GlobalEnv)
  out <- modelsummary(mod, "data.frame")
  expect_true(all(out$term[11:14] == c("test5", "test6", "test3", "test4")))
  rm("glance_custom.glm", envir = .GlobalEnv)
})

#################
#  tidy_custom  #
#################
test_that("tidy_custom.glm", {
  # not sure why this fails on older versions
  testthat::skip_if(getRversion() < '4.0.0')
  tidy_custom.glm <- function(x) {
    data.frame(
      term = names(stats::coef(x)),
      estimate = ifelse(stats::coef(x) > 0, "+", "-")
    )
  }
  # beware of testthat scoping issue
  assign("tidy_custom.glm", tidy_custom.glm, envir=.GlobalEnv)
  out <- modelsummary(mod, 
    output = "data.frame", 
    gof_omit = "",
    statistic = NULL)
  expect_equal(unname(out[["(1)"]]), c("-", "+"))
  rm("tidy_custom.glm", envir=.GlobalEnv)
})


test_that("tidy_custom.glm partial term names", {
  tidy_custom.glm <- function(x) {
    data.frame(
      term = c("(Intercept)", "hp"),
      estimate = ifelse(stats::coef(x) > 0, 4, pi)
    )
  }
  # beware of testthat scoping issue
  assign("tidy_custom.glm", tidy_custom.glm, envir=.GlobalEnv)
  out <- modelsummary(mod, 
    output = "data.frame", 
    gof_omit = "",
    statistic = NULL)
  expect_equal(unname(out[["(1)"]]), c("3.142", "0.307"))
  rm("tidy_custom.glm", envir=.GlobalEnv)
})


test_that("tidy_custom.glm wrong term names", {
  tidy_custom.glm <- function(x) {
    data.frame(
      term = c("bad2", "bad1"),
      estimate = ifelse(stats::coef(x) > 0, 4, pi)
    )
  }
  # beware of testthat scoping issue
  assign("tidy_custom.glm", tidy_custom.glm, envir=.GlobalEnv)
  expect_warning(modelsummary(mod, 
    output = "data.frame", 
    gof_omit = "",
    statistic = NULL))
  rm("tidy_custom.glm", envir=.GlobalEnv)
})


#################
#  tidy.custom  #
#################
test_that("tidy.custom", {
  options(modelsummary_get = "broom")
  mod_custom <- mod
  class(mod_custom) <- c("custom", class(mod_custom))
  tidy.custom <- function(x, ...) {
    data.frame(
      term = c("a", "b"),
      estimate = 1:2,
      std.error = 2:3
    ) 
  }
  glance.custom <- function(x, ...) {
    data.frame("custom" = "model")
  }
  # beware of testthat scoping issues
  assign("tidy.custom", tidy.custom, envir = .GlobalEnv)
  assign("glance.custom", glance.custom, envir = .GlobalEnv)
  tab <- modelsummary(mod_custom, output = "dataframe")
  expect_equal(dim(tab), c(6, 4))
  rm("tidy.custom", envir = .GlobalEnv)
  rm("glance.custom", envir = .GlobalEnv)
  options(modelsummary_get = "easystats")
})
