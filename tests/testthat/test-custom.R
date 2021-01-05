
mod <- glm(am ~ mpg, mtcars, family = binomial)

test_that("glance_custom.glm", {
  glance_custom.glm <- function(x) {
    data.frame("test"=1.54, "test2"="lkkd", "test3"=as.integer(2),
               "test4"=TRUE)
  }
  # beware of testthat scoping issue
  assign("glance_custom.glm", glance_custom.glm, envir=.GlobalEnv)
  out <- modelsummary(mod, "data.frame")
  expect_equal(dim(out), c(12, 4))
  rm("glance_custom.glm", envir=.GlobalEnv)
})

test_that("tidy_custom.glm", {
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
  expect_equal(unname(out[["Model 1"]]), c("-", "+"))
  rm("tidy_custom.glm", envir=.GlobalEnv)
})

test_that("tidy.custom", {
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
  assign("tidy.custom", tidy.custom, envir=.GlobalEnv)
  assign("glance.custom", glance.custom, envir=.GlobalEnv)
  tab <- modelsummary(mod_custom, output = "dataframe")
  expect_equal(dim(tab), c(5, 4))
  rm("tidy.custom", envir=.GlobalEnv)
  rm("glance.custom", envir=.GlobalEnv)
})
