#' Allow users to override uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
get_vcov <- function(model, vcov = NULL, ...) {
    UseMethod("get_vcov", model)
}


#' @export
#' @keywords internal
get_vcov.default <- function(model, vcov = NULL, ...) {

  if (all(sapply(vcov, is.null))) return(NULL)

  dots <- list(...)

  # needed for logic tests
  out <- mat <- NULL

  if (is.null(vcov)) {
    return(NULL)

  } else if (isTRUE(checkmate::check_atomic_vector(vcov, names = "named"))) {
    out <- data.frame(term = names(vcov), std.error = vcov, stringsAsFactors = FALSE)
    return(out)

  } else if (isTRUE(checkmate::check_character(vcov, len = 1))) {
    out <- insight::get_varcov(model, vcov = vcov, vcov_args = dots, component = "all")

  } else if (isTRUE(checkmate::check_formula(vcov))) {
    dots[["cluster"]] <- vcov
    if (inherits(model, "fixest")) {
      out <- try(
        insight::get_varcov(model, vcov = "vcovCL", vcov_args = dots, component = "all"),
        silent = TRUE)
    } else {
      out <- try(
        insight::get_varcov(model, vcov = "vcovCL", vcov_args = dots, component = "all"),
        silent = TRUE)
    }
    if (inherits(out, "try-error")) {
      msg <- attr(out, "condition")$message
      if (grepl("Unable to extract", msg)) {
        msg <- paste(msg, "Note that the cluster variable in the formula cannot include missing `NA` observations.")
      }
      msg <- gsub("\\n", " ", msg)
      stop(insight::format_message(msg), call. = FALSE)
    }

  } else if (isTRUE(checkmate::check_function(vcov))) {
    args <- c(list(model), dots)
    out <- try(do.call("vcov", args), silent = TRUE)

  } else if (isTRUE(checkmate::check_matrix(vcov))) {
    out <- vcov
  }

  # lme4::lmer
  if (inherits(out, "dpoMatrix") || inherits(out, "vcovCR")) {
      out <- as.matrix(out)
  }

  if (is.matrix(out)) {
    return(out)

  } else {
    msg <- "Unable to extract a variance-covariance matrix from model of class `%s`."
    msg <- insight::format_message(sprintf(msg, class(model)[1]))
    stop(msg, call. = FALSE)
  }
}
