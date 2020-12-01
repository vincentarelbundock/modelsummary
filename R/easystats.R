#' extract gof using the easystats `performance` package
#'
#' @keywords internal
glance_easystats <- function(model, ...) {
  if (!check_dependency("performance")) {
    return(NULL)
  }
  out <- performance::model_performance(model)
  out <- insight::standardize_names(out, style="broom")
  return(out)
}

#' extract estimates using the easystats `parameters` package
#'
#' @keywords internal
tidy_easystats <- function(model, ...) {
  if (!check_dependency("parameters")) {
    return(NULL)
  }
  out <- parameters::model_parameters(model, ...)
  out <- parameters::standardize_names(out, style="broom")
  return(out)
}
