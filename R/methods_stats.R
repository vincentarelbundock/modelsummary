#' @inherit glance_custom_internal
#' @keywords internal
glance_custom_internal.lm <- function(x, vcov_type = NULL, gof = NULL, ...) {
  # glm also inherits from lm

  out <- data.frame(row.names = "firstrow")

  # rename aliases
  if (!is.null(vcov_type)) {
    if (vcov_type == "Stata") {
      vcov_type <- "HC1"
    } else if (vcov_type == "Robust") {
      vcov_type <- "HC3"
    }
  }

  # default glance
  if ((is.null(vcov_type) || vcov_type %in% c("Classical", "Constant", "IID", "Standard", "Default"))) {
    # F-statistic
    if (inherits(gof, "data.frame") && "statistic" %in% colnames(gof)) {
      out[["F"]] <- gof$statistic
    } else {
      fstat <- try(lmtest::waldtest(x, vcov = stats::vcov)$F[2], silent = TRUE)
      if (inherits(fstat, "numeric")) {
        out[["F"]] <- fstat
      }
    }
    # RMSE is not extracted by {broom} but we want it
    if (!"rmse" %in% colnames(gof)) {
        out[["rmse"]] <- stats::sigma(x)
    }
  }

  # sandwich HC
  if (!is.null(vcov_type) &&
      isTRUE(check_dependency("lmtest")) &&
      isTRUE(check_dependency("sandwich")) &&
      vcov_type %in% c("HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")) {
      fun <- function(k) sandwich::vcovHC(k, type = vcov_type)
      fstat <- try(lmtest::waldtest(x, vcov = fun)$F[2], silent = TRUE)
      if (inherits(fstat, "numeric")) {
        out[["F"]] <- fstat
      }
  }

  row.names(out) <- NULL
  return(out)
}
