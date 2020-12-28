#' Allow users to override uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @noRd
#' @return a numeric vector of test statistics
extract_vcov <- function(model, vcov, conf_level=NULL) {

  # needed for logic tests
  out <- mat <- NULL

  # vcov = character
  if (is.character(vcov) && length(vcov) == 1) {

    assert_dependency("sandwich")
    if (vcov == "stata") {
      vcovtype <- "HC1"
    } else if (vcov == "robust") {
      vcovtype <- "HC3"
    } else {
      vcovtype <- vcov
    }

    mat <- try(sandwich::vcovHC(model, type=vcovtype), silent=TRUE)

    if (!inherits(mat, "matrix")) {
      msg <- "Unable to extract a variance-covariance matrix of type %s from model of class %s. The uncertainty estimates are unadjusted."
      warning(sprintf(msg, vcov, class(model)[1]))
      return(NULL)
    }
  }

  # vcov = formula (clusters)
  if (isTRUE(checkmate::check_formula(vcov))) {
    assert_dependency("sandwich")
    mat <- try(sandwich::vcovCL(model, cluster=vcov), silent=TRUE)

    if (!inherits(mat, "matrix")) {
      msg <- "Unable to extract a clustered variance-covariance matrix from model of class %s. The uncertainty estimates are unadjusted."
      warning(sprintf(msg, class(model)[1]))
      return(NULL)
    }
  }

  # vcov = function
  if (is.function(vcov)) {
    mat <- try(vcov(model), silent=TRUE)

    # lme4::lmer
    if (inherits(mat, "dpoMatrix")) { 
      mat <- as.matrix(mat)
    }

    if (!inherits(mat, "matrix")) {
      msg <- "Unable to use function to extract a variance-covariance matrix from model of class %s. The uncertainty estimates are unadjusted."
      warning(sprintf(msg, class(model)[1]))
      return(NULL)
    }
  }

  # vcov = matrix
  if (is.matrix(vcov)) {
    mat <- vcov
  }

  # vcov = atomic vector
  if (isTRUE(checkmate::check_atomic_vector(vcov, names="named"))) {
    out <- data.frame(term=names(vcov), std.error=vcov)

    # factor -> character (important for R<4.0.0)
    for (i in seq_along(out)) {
      if (is.factor(out[[i]])) {
        out[[i]] <- as.character(out[[i]])
      }
    }
    return(out)
  }

  # try lmtest::coeftest 
  if (is.matrix(mat)) {
    out <- get_coeftest(model, mat, conf_level)

    # lmtest::coeftest worked
    if (inherits(out, "data.frame")) {
      return(out)
    }

    # lmtest::coeftest failed. adjust std.error manually
    if (!inherits(out, "data.frame")) {
      out <- sqrt(base::diag(mat))
      out <- data.frame(term=colnames(mat), std.error=out)
      msg <- "The `lmtest::coeftest` function does not seem to produce a valid result when applied to a model of class %s. Only `std.error` can be adjusted."
      warning(sprintf(msg, class(model)[1]))
      return(out)
    }
  }

  msg <- "Unable to extract a clustered variance-covariance matrix from model of class %s. The uncertainty estimates are unadjusted."
  warning(sprintf(msg), class(model)[1])
  return(NULL)
}


get_coeftest <- function(model, vcov, conf_level) {

  if (!check_dependency("lmtest")) return(NULL)

  gof <- try(
    lmtest::coeftest(model, vcov.=vcov), silent=TRUE)

  gof_ci <- try(
    lmtest::coefci(model, vcov.=vcov, level=conf_level),
    silent=TRUE)

  if (!inherits(gof, "try-error")) {
    gof <- as.data.frame(unclass(gof))
    colnames(gof) <- c("estimate", "std.error", "statistic", "p.value")
    gof$term <- row.names(gof)
  }

  if (!inherits(gof_ci, "try-error") &&
      !inherits(gof, "try-error")) {
    gof_ci <- as.data.frame(unclass(gof_ci))
    colnames(gof_ci) <- c("conf.low", "conf.high")
    gof_ci$term <- row.names(gof_ci)
    gof <- merge(gof, gof_ci, by="term")
  }

  if (inherits(gof, "try-error")) {
    gof <- NULL
  }

  return(gof)
}

