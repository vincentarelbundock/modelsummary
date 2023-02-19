library(tinytest)
options("tinyviztest_device" = "svglite")
requiet <- function(package) {
  testthat::skip_if_not_installed(package)
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
