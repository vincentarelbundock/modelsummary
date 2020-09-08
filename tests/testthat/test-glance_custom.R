context('glance_custom')

library(modelsummary)

test_that("fixed value glance_custom.glm", {
  glance_custom.glm <- function(x) {
    data.frame("test"=1.54, "test2"="lkkd", "test3"=as.integer(2),
               "test4"=TRUE)
  }

  # hack to fix testthat scoping issue
  assign("glance_custom.glm", glance_custom.glm, envir=.GlobalEnv)

  mod <- glm(am ~ mpg, mtcars, family = binomial)
  out <- msummary(mod, "data.frame")
  expect_equal(dim(out), c(12, 4))

  # hack to fix testthat scoping issue
  rm("glance_custom.glm", envir=.GlobalEnv)
})

