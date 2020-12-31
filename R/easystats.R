#' extract gof using the easystats `performance` package
#'
#' @noRd
glance_easystats <- function(model, ...) {
  error_msg <- utils::capture.output(out <- performance::model_performance(model))

  out <- insight::standardize_names(out, style="broom")
  mi <- insight::model_info(model)

  # nobs
  if ("n_obs" %in% names(mi)) {
    out$nobs <- mi$n_obs
  }

  # logLik
  ll <- try(stats::logLik(model), silent=TRUE)
  if (!inherits(ll, "try-error")) {
    out$logLik <- as.numeric(ll[1])
  }


  return(out)
}

#' extract estimates using the easystats `parameters` package
#'
#' @noRd
tidy_easystats <- function(model, ...) {
  msg <- utils::capture.output(out <- parameters::model_parameters(model, ...))
  out <- parameters::standardize_names(out, style="broom")
  return(out)
}
