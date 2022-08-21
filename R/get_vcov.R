#' Allow users to override uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
get_vcov <- function(model, vcov = NULL, conf_level = NULL, ...) {
    UseMethod("get_vcov", model)
}



#' @export
#' @keywords internal
get_vcov.default <- function(model, vcov = NULL, conf_level = NULL, ...) {

  if (all(sapply(vcov, is.null))) return(NULL)

  dots <- list(...)

  # needed for logic tests
  out <- mat <- NULL

  if (is.null(vcov)) {
    return(NULL)

  } else if (isTRUE(checkmate::check_atomic_vector(vcov, names = "named"))) {
    out <- data.frame(term = names(vcov), std.error = vcov)

    # factor -> character (important for R<4.0.0)
    for (i in seq_along(out)) {
      if (is.factor(out[[i]])) {
        out[[i]] <- as.character(out[[i]])
      }
    }
    return(out)

  } else if (isTRUE(checkmate::check_character(vcov, len = 1))) {
    mat <- insight::get_varcov(model, vcov = vcov, vcov_args = dots)

  } else if (isTRUE(checkmate::check_formula(vcov))) {
    dots[["cluster"]] <- vcov
    mat <- insight::get_varcov(model, vcov = "vcovCL", vcov_args = dots)

  } else if (isTRUE(checkmate::check_function(vcov))) {
    mat <- try(vcov(model), silent = TRUE)

  } else if (isTRUE(checkmate::check_matrix(vcov))) {
    mat <- vcov
  }

  # lme4::lmer
  if (inherits(mat, "dpoMatrix") || inherits(mat, "vcovCR")) {
      mat <- as.matrix(mat)
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
      out <- data.frame(term = colnames(mat), std.error = out)

      msg <- insight::format_message(sprintf("The `lmtest::coeftest` function does not seem to produce complete results when applied to a model of class %s. Only the standard errors have been adjusted. p values and confidence intervals may not be correct.", class(model)[1]))
      warning(msg, call. = FALSE)
      return(out)
    }
  }

  msg <- "Unable to extract a variance-covariance matrix from model of class `%s`."
  msg <- insight::format_message(sprintf(msg, class(model)[1]))
  stop(msg, call. = FALSE)
}


get_coeftest <- function(model, vcov, conf_level) {

  if (!isTRUE(check_dependency("lmtest"))) return(NULL)

  gof <- tryCatch(
      {
          tmp <- lmtest::coeftest(model, vcov. = vcov)
          tmp <- unclass(tmp)
          tmp <- as.data.frame(tmp)
          tmp <- stats::setNames(tmp, c("estimate", "std.error", "statistic", "p.value"))
          tmp$term <- row.names(tmp)
          row.names(tmp) <- NULL
          tmp
      },
      error = function(e) NULL)

  # coeftest returns only se for coefficients and not for intercepts in MASS::polr
  if (!is.data.frame(gof) || nrow(gof) != nrow(vcov)) {
    return(NULL)
  }

  gof_ci <- try(
    lmtest::coefci(model, vcov. = vcov, level = conf_level),
    silent = TRUE)

  if (!inherits(gof_ci, "try-error") && !inherits(gof, "try-error")) {
    gof_ci <- as.data.frame(unclass(gof_ci))
    colnames(gof_ci) <- c("conf.low", "conf.high")
    gof_ci$term <- row.names(gof_ci)
    gof <- merge(gof, gof_ci, by = "term")
  }

  if (inherits(gof, "try-error")) {
    gof <- NULL
  }

  return(gof)
}


#' internal function
#'
#' @param v a string or formula describing the standard error type
#' @keywords internal
get_vcov_type <- function(vcov) {

  # user-supplied std.error names
  if (!is.null(names(vcov))) return(names(vcov))

  get_vcov_type_inner <- function(v) {

    if (is.character(v)) {
      if (v %in% c("robust", "classical", "stata", "classical", "constant", "panel-corrected", "Andrews", "outer-product")) {
         out <- tools::toTitleCase(v)
       } else if (v == "NeweyWest") {
         out <- "Newey-West"
       } else {
         out <- toupper(v)
       }
    } else if (inherits(v, "formula")) {
      out <- paste("by:", gsub("\\+", "\\&", gsub(":", "\\ & ", as.character(v)[2])))
    } else if (is.null(v)) {
      out <- NULL
    } else if (is.function(v)) {
      out <- "function"
    } else if (is.matrix(v)) {
      out <- "matrix"
    } else if (checkmate::test_atomic_vector(v)) {
      out <- "vector"
    } else {
      out <- NULL
    }
    return(out)
  }

  vcov_type <- sapply(vcov, get_vcov_type_inner)

  return(vcov_type)
}


