mod <- list()
mod$OLS <- lm(hp ~ drat, data = mtcars)
mod$Logit <- glm(am ~ drat + hp, data = mtcars, family = binomial())

test_that("rounding 5 decimals", {

  raw <- modelsummary(mod, fmt = '%.5f', output="dataframe")

  truth <- c("353.65253", "(76.04873)", "-57.54523", "(20.92205)", "", "")
  expect_equal(truth, unname(raw[[4]])[1:6])

  truth <- c("-29.07608", "(12.41692)", "7.30978", "(3.04660)", "0.01079", "(0.00933)")
  expect_equal(truth, unname(raw[[5]])[1:6])

})

test_that("rounding internal function", {
  expect_equal(modelsummary:::rounding(2), "2.000")
  expect_equal(modelsummary:::rounding("2"), "2")
})



test_that("fmt integer respects options(OutDec)", {
  options(OutDec=",")
  known <- c("353,6525", "(76,0487)", "-57,5452", "(20,9221)", "", "", "32",
             "0,201", "0,175", "359,2", "363,6", "-176,588", "7,565")
  x <- modelsummary(mod, fmt=4, output="data.frame")
  expect_equal(x$OLS, known)
  options(OutDec=".")
})

test_that("fmt character", {
  known <- c("353.6525", "(76.0487)", "-57.5452", "(20.9221)", "", "", "32",
             "0.201", "0.175", "359.2", "363.6", "-176.588", "7.565")
  x <- modelsummary(mod, fmt="%.4f", output="data.frame")
  expect_equal(x$OLS, known)
})

test_that("fmt function", {
 known <- c("353.6525", "(76.0487)", "-57.5452", "(20.9221)", "", "", "32",
            "0.201", "0.175", "359.2", "363.6", "-176.588", "7.565")
  z <- modelsummary(mod, fmt=function(x) sprintf("%.4f", x), output="data.frame")
  expect_equal(known, z$OLS)
})
