library(testthat)
library(modelsummary)

if (isTRUE(Sys.getenv("R_NOT_CRAN") == "true")) {
    test_check("modelsummary")
}
