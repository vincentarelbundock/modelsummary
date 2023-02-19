rm(list = ls())

library(tinytest)

options(width = 10000)
options("tinyviztest_device" = "svglite")

requiet <- function(package) {
  suppressMessages(suppressWarnings(suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )))
}

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}

compare_files <- function(x, y) {
  known <- digest::digest(x, file = TRUE)
  unknown <- digest::digest(y, file = TRUE)
  expect_equivalent(known, unknown)
}