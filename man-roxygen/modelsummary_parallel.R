#' @section Parallel computation:
#'
#' It can take a long time to compute and extract summary statistics from
#' certain models (e.g., Bayesian). In those cases, users can parallelize the
#' process. Since parallelization occurs at the model level, no speedup is
#' available for tables with a single model. Users on mac or linux can launch
#' parallel computation using the built-in `parallel` package. All they need to
#' do is supply a `mc.cores` argument which will be pushed forward to the
#' `parallel::mclapply` function:
#'
#' ```{r, eval = FALSE}
#' modelsummary(model_list, mc.cores = 5)
#' ```
#'
#' The command above will often -- but not always -- work for Windows users.
#' Alternatively, users can also use the `future` parallelization framework by
#' installing the `future.apply` package and by calling the `plan()` function.
#' Example:
#' 
#' ```{r, eval = FALSE}
#' library(future.apply)
#' plan("multisession")
#' modelsummary(model_list)
#' ```
#'
