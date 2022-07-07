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

  # log-likelihood (broken with `gam`, which inherits from `glm`)
  if (!"gam" %in% class(x)) {
      ll <- tryCatch(
          insight::get_loglikelihood(x),
          error = function(e) NULL)
      out[["logLik"]] <- ll
  }

  row.names(out) <- NULL
  return(out)
}


#' @inherit get_vcov
#' @keywords internal
get_vcov.mlm <- function(model, vcov = NULL, conf_level = NULL, ...) {
    out <- parameters::parameters(model, vcov = vcov, ci = conf_level, ...)
    out <- parameters::standardize_names(out, style = "broom")
    return(out)
}
