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
#' All users can also use the `future.apply` package to parallelize model summaries.
#' For example, to use 4 cores to extract results:
#' 
#' ```{r, eval = FALSE}
#' library(future.apply)
#' plan(multicore, workers = 4)
#' options("modelsummary_future" = TRUE)
#' modelsummary(model_list)
#' ```
#'
#' Note that the "multicore" plan only parallelizes under mac or linux. Windows
#' users can use `plan(multisession)` instead. However, note that the first
#' time `modelsummary()` is called under multisession can be a fair bit longer,
#' because of extra costs in passing data to and loading required packages on
#' to workers. Subsequent calls to `modelsummary()` will often be much faster.
#'
#' Some users have reported difficult to reproduce errors when using the
#' `future` package with some packages. The `future` parallelization in
#' `modelsummary` can be disabled by calling:
#'
#' `options("modelsummary_future" = FALSE)`
