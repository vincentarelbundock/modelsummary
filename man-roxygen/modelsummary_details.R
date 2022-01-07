#' 
#' ## `output`
#'
#' The `modelsummary_list` output type is a lightweight representation of the
#' model results. The `modelsummary` function can export to this format by
#' setting the `output` argument, and it can accept objects of this format
#' as input models to create a table. This can be useful to save raw
#' results, in order to print a table later, without having to save and
#' extract from the entire model object. Note that the confidence intervals
#' are only stored in a `modelsummary_list` if explicitly requested:
#'
#' `backup <- modelsummary(models, output = "modelsummary_list", statistic = "conf.int")`
#' `modelsummary(backup)`
#'
#' When a file name with a valid extension is supplied to the `output` argument,
#' the table is written immediately to file. If you want to customize your table
#' by post-processing it with an external package, you need to choose a
#' different output format and saving mechanism. Unfortunately, the approach
#' differs from package to package:
#'
#' * `gt`: set `output="gt"`, post-process your table, and use the `gt::gtsave` function.
#' * `kableExtra`: set `output` to your destination format (e.g., "latex", "html", "markdown"), post-process your table, and use `kableExtra::save_kable` function.
#'
#' ## `vcov`
#'
#' To use a string such as "robust" or "HC0", your model must be supported
#' by the `sandwich` package. This includes objects such as: lm, glm,
#' survreg, coxph, mlogit, polr, hurdle, zeroinfl, and more.
#'
#' NULL, "classical", "iid", and "constant" are aliases which do not modify
#' uncertainty estimates and simply report the default standard errors stored
#' in the model object.
#'
#' One-sided formulas such as `~clusterid` are passed to the `sandwich::vcovCL`
#' function.
#'
#' Matrices and functions producing variance-covariance matrices are first
#' passed to `lmtest`. If this does not work, `modelsummary` attempts to take
#' the square root of the diagonal to adjust "std.error", but the other
#' uncertainty estimates are not be adjusted.
#'
#' Numeric vectors are formatted according to `fmt` and placed in brackets.
#' Character vectors printed as given, without parentheses.
#'
#' If your model type is supported by the `lmtest` package, the
#' `vcov` argument will try to use that package to adjust all the
#' uncertainty estimates, including "std.error", "statistic", "p.value", and
#' "conf.int". If your model is not supported by `lmtest`, only the "std.error"
#' will be adjusted by, for example, taking the square root of the matrix's
#' diagonal.
