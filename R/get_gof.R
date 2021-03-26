#' Extract model gof A mostly internal function with some potential uses
#' outside.
#'
#' @inheritParams get_estimates
#' @export
get_gof <- function(model, ...) {

  get_priority <- getOption("modelsummary_get", default = "broom")
  checkmate::assert_choice(get_priority, choices = c("broom", "easystats", "parameters", "performance", "all"))

  warning_msg <- NULL

  # broom priority
  if (get_priority == "broom") {
    gof <- get_gof_broom(model, ...)
    if (is.character(gof)) {
      warning_msg <- c(warning_msg, gof)
      gof <- get_gof_performance(model, ...)
      if (is.character(gof)) {
        warning_msg <- c(warning_msg, gof)
      }
    }

  # performance priority
  } else if (get_priority == "performance") {
    gof <- get_gof_performance(model, ...)
    if (is.character(gof)) {
      warning_msg <- c(warning_msg, gof)
      gof <- get_gof_broom(model, ...)
      if (is.character(gof)) {
        warning_msg <- c(warning_msg, gof)
      }
    }

  # combine broom + performance
  } else {
    gof1 <- get_gof_broom(model, ...)
    gof2 <- get_gof_performance(model, ...)
    if (inherits(gof1, "data.frame") && inherits(gof2, "data.frame")) {
      gof <- bind_cols(gof1, gof2)
    } else if (inherits(gof1, "data.frame")) {
      gof <- gof1
      warning_msg <- c(warning_msg, gof2)
    } else if (inherits(gof2, "data.frame")) {
      gof <- gof2
      warning_msg <- c(warning_msg, gof1)
    }
  }

  # lm model: include F-stat by default
  # glm also inherits from lm
  if (isTRUE(class(model)[1] == "lm") && 
      "statistic" %in% colnames(gof)) {
    gof$F <- gof$statistic
  }

  if (inherits(gof, "data.frame")) {
    return(gof)
  }

  stop(sprintf(
'`modelsummary could not extract the required information from a model
of class "%s". The package tried a sequence of 2 helper functions to extract
goodness-of-fit statistics:

broom::glance(model)
performance::model_performance(model)

To draw a table, one of these commands must return a one-row `data.frame`.
The `modelsummary` website explains how to summarize unsupported models or
add support for new models yourself:

https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

These errors messages were generated during extraction:
%s',
class(model)[1], paste(warning_msg, collapse = "\n")))
}


#' Extract goodness-of-fit statistics from a single model using the
#' `broom` package or another package with package which supplies a
#' method for the `generics::glance` generic.
#'
#' @keywords internal
get_gof_broom <- function(model, ...) {

  out <- suppressWarnings(try(
    broom::glance(model, ...),
    silent=TRUE))

  if (!inherits(out, "data.frame")) {
    return("`broom::glance(model)` did not return a data.frame.")
  }

  if (nrow(out) > 1) {
    return("`broom::glance(model)` returned a data.frame with more than 1 row.")   
  }

  return(out)
}


#' Extract goodness-of-fit statistics from a single model using
#' the `performance` package
#'
#' @keywords internal
get_gof_performance <- function(model, ...) {

  # select appropriate metrics to compute
  if ("metrics" %in% names(list(...))) {
    out <- suppressWarnings(try(
      performance::model_performance(model, ...)))
  } else {
    # stan models: r2_adjusted is veeeery slow
    if (inherits(model, "stanreg") ||
        inherits(model, "brmsfit") ||
        inherits(model, "stanmvreg")) {
      # this is the list of "common" metrics in `performance`
      # documentation, but their code includes R2_adj, which produces
      # a two-row glance and gives us issues.
      metrics <- c("LOOIC", "WAIC", "R2", "RMSE")
    } else {
      metrics <- "all"
    }
    out <- suppressWarnings(try(
      performance::model_performance(model, metrics = metrics, ...)))
  }

  # sanity
  if (!inherits(out, "data.frame")) {
    return("`performance::model_performance(model)` did not return a data.frame.")
  }

  if (nrow(out) > 1) {
    return("`performance::model_performance(model)` returned a data.frame with more than 1 row.")   
  }

  # cleanup
  out <- insight::standardize_names(out, style="broom")

  # nobs
  mi <- insight::model_info(model)
  if ("n_obs" %in% names(mi)) {
    out$nobs <- mi$n_obs
  }

  return(out)
}
