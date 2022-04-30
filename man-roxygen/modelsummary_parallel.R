#' @section Parallel computation:
#'
#' It can take a long time to compute and extract summary statistics from certain models (e.g., Bayesian). In those cases, users can parallelize the process. Since parallelization occurs at the model level, no speedup is available for tables with a single model. To use parallel computation, all users have to do is load the `future.apply` package and call the `plan()` function. Example:
#' 
#' ```{r, eval = FALSE}
#' library(future.apply)
#' plan("multisession")
#' modelsummary(model_list)
#' ```
#' 
