library(testthat)
library(modelsummary)
library(nnet)
random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}

test_check("modelsummary")
