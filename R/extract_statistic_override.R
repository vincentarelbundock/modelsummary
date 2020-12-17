#' Allow users to override uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
extract_vcov <- function(model, vcov, conf_level=NULL) {

  # needed for logic tests
  out <- mat <- NULL

  # character shortcuts
  regex = "^robust$|^HC$|^HC0$|^HC1$|^HC2$|^HC3$|^HC4$|^HC4m$|^HC5$|^stata$|^classical$|^constant$|^iid$"
  flag <- checkmate::check_character(vcov, len=1, pattern=regex)
  if (isTRUE(flag)) {
    assert_dependency("sandwich")
    if (vcov %in% c("classical", "constant", "iid")) {
      mat <- stats::vcov(model)
    } else {
      if (vcov == "stata") {
        vcov <- "HC1"
      } else if (vcov == "robust") {
        vcov <- "HC3"
      }
      mat <- sandwich::vcovHC(model, type=vcov)
    }
  }

  # formula for clusters
  flag <- checkmate::check_formula(vcov)
  if (isTRUE(flag)) {
    assert_dependency("sandwich")
    mat <- sandwich::vcovCL(model, cluster=vcov)
  }

  # function is expected to return a covariance matrix
  if (is.function(vcov)) {
    mat <- vcov(model)
  }

  # matrix
  if (is.matrix(vcov)) {
    mat <- vcov
  }

  # lmtest attempt and manual fallback
  if (is.matrix(mat)) {
    out <- get_coeftest(model, mat, conf_level)

    if (inherits(out, "data.frame")) {
      return(out)
    } else {
      out <- sqrt(base::diag(out))
      out <- data.frame(term=names(out), std.error=out)
      warning("Your model does not seem to be supported by the `lmtest` package. Only `std.error` can be adjusted.")
      return(out)
    }
  }

  # atomic vector
  flag <- checkmate::check_atomic_vector(vcov, names="named")
  if (isTRUE(flag)) {
    out <- data.frame(term=names(vcov), 
                      std.error=vcov)
    # factor -> character (important for R<4.0.0)
    for (i in seq_along(out)) {
      if (is.factor(out[[i]])) {
        out[[i]] <- as.character(out[[i]])
      }
    }
    return(out)
  }

  stop("Could not retrieve a valid variance-covariance matrix using the function supplied in `vcov`.")
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

