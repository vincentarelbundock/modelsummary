run <- FALSE
pkg <- requireNamespace("tinytest", quietly = TRUE)
home <- grepl("(?i)vince", Sys.info()["nodename"]) || identical(Sys.getenv("R_NOT_CRAN"), "true")
if (pkg && home && run) {
  tinytest::test_package("marginaleffects")
}
