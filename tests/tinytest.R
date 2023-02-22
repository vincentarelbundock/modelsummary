
if (isTRUE(Sys.getenv("R_NOT_CRAN") == "true")) {
  if (requireNamespace("tinytest", quietly = TRUE)) {
    tinytest::test_package("modelsummary")
  }
}