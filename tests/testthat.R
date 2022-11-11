library(testthat)
library(modelsummary)

if (isTRUE(Sys.getenv("R_NOT_CRAN") == "yes")) {
    test_check("modelsummary")
}
