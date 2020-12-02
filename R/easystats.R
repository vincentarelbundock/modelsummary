#' extract gof using the easystats `performance` package
#'
#' @keywords internal
glance_easystats <- function(model, ...) {
  if (!check_dependency("performance")) {
    return(NULL)
  }
  out <- performance::model_performance(model)
  out <- insight::standardize_names(out, style="broom")
  mi <- insight::model_info(model)
  if ("n_obs" %in% names(mi)) {
    out$nobs <- mi$n_obs
  }
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
