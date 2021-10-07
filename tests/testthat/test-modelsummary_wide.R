# modelsummary_wide is superseded and a warning is issued every time a user calls it
# breaking changes can (and will likely) happen

# make_data <- function(response = c("A", "B", "C")) {
#   var1 <- sample(response, replace = T, size=100)
#   var2 <- sample(c(0,1), size=100, replace=T)
#   var3 <- rnorm(100, mean=10, sd=2)
#   var1 <- factor(var1)
#   df1 <- data.frame(var1, var2, var3)
#   df1
# }

# test_that("statistic = NULL does not raise an error", {
#   testthat::skip_if_not_installed("nnet")
#   df1 <- make_data()
#   df2 <- make_data()
#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   invisible(capture.output(m2 <- nnet::multinom(var1~var2, data=df2)))
#   models <- list("a" = m1, "b" = m2)
#   tab <- suppressWarnings(modelsummary_wide(models, output = "dataframe", statistic = NULL))
#   expect_equal(c(4, 7), dim(tab))
# })

# test_that("model names", {
#   testthat::skip_if_not_installed("nnet")
#   df1 <- make_data()
#   df2 <- make_data()
#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   invisible(capture.output(m2 <- nnet::multinom(var1~var2, data=df2)))
#   models <- list("a" = m1, "b" = m2)
#   tab <- suppressWarnings(modelsummary_wide(models, output = "dataframe", statistic="conf.int"))
#   truth <- c("part", "term", "statistic", "a B", "a C", "b B", "b C")
#   expect_equal(truth, colnames(tab))
# })

# test_that("no group", {
#   mod <- list(
#     lm(hp ~ mpg, mtcars),
#     lm(hp ~ mpg + drat + vs, mtcars)
#   )
#   expect_error(suppressWarnings(modelsummary_wide(mod)))
# })

# test_that("nnet::multinom one model (factor DV)", {
#   testthat::skip_if_not_installed("nnet")
#   df <- make_data()
#   invisible(capture.output(mod <- nnet::multinom(var1~var2, data=df)))
#   tmp <- suppressWarnings(modelsummary_wide(mod, output="data.frame"))
#   expect_s3_class(tmp, "data.frame")
#   expect_equal(dim(tmp), c(6, 5))
#   tmp <- suppressWarnings(modelsummary_wide(mod, output="data.frame", stacking="vertical"))
#   expect_s3_class(tmp, "data.frame")
#   expect_equal(dim(tmp), c(6, 5))
#   # no model label in gof for single model
#   truth <- c("Model 1 (Intercept)", "Model 1 (Intercept)", "Model 1 var2", "Model 1 var2", "Num.Obs.", "AIC")
#   expect_equal(tmp$term, truth)
# })


# test_that("2 models: horizontal and vertical", {
#   testthat::skip_if_not_installed("nnet")
#   df1 <- make_data()
#   df2 <- make_data()
#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   invisible(capture.output(m2 <- nnet::multinom(var1~var2, data=df2)))
#   tmp <- suppressWarnings(modelsummary_wide(list(m1, m2), output="data.frame", stacking="horizontal"))
#   expect_s3_class(tmp, "data.frame")
#   expect_equal(dim(tmp), c(6, 7))
#   tmp <- suppressWarnings(modelsummary_wide(list(m1, m2), output = "data.frame", stacking = "vertical"))
#   expect_s3_class(tmp, "data.frame")
#   expect_equal(dim(tmp), c(8, 5))
# })


# test_that("2 models with different response levels", {
#   df1 <- make_data()
#   df2 <- make_data(c("B", "C", "D"))

#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   invisible(capture.output(m2 <- nnet::multinom(var1~var2, data=df2)))

#   tmp <- suppressWarnings(modelsummary_wide(list(m1, m2), output="data.frame", stacking="horizontal"))
#   expect_s3_class(tmp, "data.frame")
#   expect_equal(dim(tmp), c(6, 7))

#   tmp <- suppressWarnings(modelsummary_wide(list(m1, m2), output="data.frame", stacking="vertical"))
#   expect_s3_class(tmp, "data.frame")
#   expect_equal(dim(tmp), c(8, 6))
# })

# test_that("single model: no model names", {
#   df1 <- make_data()
#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   tmp <- suppressWarnings(modelsummary_wide(m1, output="data.frame"))
#   truth <- c("part", "term", "statistic", "B", "C")
#   expect_equal(truth, colnames(tmp))
# })

# test_that("2 models w/ horizontal stack", {
#   testthat::skip_if_not_installed("nnet")

#   var1 <- sample(c('A', 'B', 'C'), replace = T, size=100)
#   var2 <- sample(c(0,1), size=100, replace=T)
#   var3 <- rnorm(100, mean=10, sd=2)
#   var1 <- factor(var1)
#   df1 <- data.frame(var1, var2, var3)

#   var1 <- sample(c('A', 'B', 'C'), replace = T, size=100)
#   var2 <- sample(c(0,1), size=100, replace=T)
#   var3 <- rnorm(100, mean=10, sd=2)
#   var1 <- factor(var1)
#   df2 <- data.frame(var1, var2, var3)

#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   invisible(capture.output(m2 <- nnet::multinom(var1~var2, data=df2)))

#   tmp <- suppressWarnings(modelsummary_wide(list(m1, m2), output="data.frame", stacking="horizontal"))
#   expect_s3_class(tmp, "data.frame")
#   # expect_equal(dim(tmp), c(20, 5))
#   # expect_equal(dim(tmp), c(16, 5))
# })
