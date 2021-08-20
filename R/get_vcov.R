#' Allow users to override uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @noRd
#' @return a numeric vector of test statistics
get_vcov <- function(model, vcov = NULL, conf_level = NULL, ...) {

  if (all(sapply(vcov, is.null))) return(NULL)

  # needed for logic tests
  out <- mat <- NULL

  msg_vcov_error <- c(
    "The variance-covariance matrix is required to adjust the standard errors. ",
    "The `vcov` argument accepts a variance-covariance matrix, a vector of standard errors, or a ",
    "function that returns one of these, such as `stats::vcov`.")

  ## BEGIN fixest catch
  if (inherits(model, "fixest") || inherits(model, "fixest_multi")) {

    ## Versions older than 0.10.0 require a different approach, since they don't
    ## support the flexible vcov argument
    fixest_0_10 = utils::packageVersion("fixest") >= "0.10.0"

    ## Match supported character args based on version
    if (fixest_0_10) {
      ## known fixest vcovs
      fixest_vcovs = c("iid", "standard",
                       "hetero", "HC1", "White",
                       "cluster", "twoway",
                       "NW", "newey_west",
                       "DK", "driscoll_kraay",
                       "conley")
      ## Equivalent aliases used by modelsummary (excl. those common to both)
      fixest_vcov_aliases = c("iid" = "classical",
                              "iid" = "constant",
                              "HC1" = "stata",
                              "NW" = "NeweyWest")
    } else {
      ## known fixest vcovs
      fixest_vcovs = c('standard', 'hetero',
                       'cluster', 'twoway', 'threeway', 'fourway')
      ## Equivalent aliases used by modelsummary  (excl. those common to both)
      fixest_vcov_aliases = c("standard" = "iid",
                              "standard" = "classical",
                              "standard" = "constant",
                              "hetero" = "stata")
    }

    is_func = class(vcov)=="function"
    is_form = class(vcov)=="formula"
    is_mat = is.matrix(vcov)

    if (!is_func && !is_form && !is_mat && vcov %in% fixest_vcov_aliases) {
      vcov = names(fixest_vcov_aliases)[which(fixest_vcov_aliases %in% vcov)]
    }

    ## if a known or compatible fixest vcov argument, use the dedicated
    ## fixest.vcov method
    if (fixest_0_10) {
      ## if a known or compatible fixest vcov argument, use the dedicated
      ## fixest.vcov method
      if (is.null(vcov) || is_func || is_form || is_mat || vcov %in% fixest_vcovs) {
        mat = vcov(model, vcov = vcov)
        out = get_coeftest(model, mat, conf_level)
        stop(return(out))
        ## else coerce to iid error for sandwich adjustment below
      } else {
        model = summary(model, vcov = "iid")
      }
      ## fixest versions older than 0.10.0 are less flexible
    } else {
      if (is_form) {
        mat = vcov(summary(model, cluster = vcov))
        out = get_coeftest(model, mat, conf_level)
        stop(return(out))
      } else if (vcov %in% fixest_vcovs) {
        mat = vcov(summary(model, se = vcov))
        out = get_coeftest(model, mat, conf_level)
        stop(return(out))
      } else {
        model = summary(model, se = "standard")
      }
    }

  }
  ## END fixest catch

  if (is.null(vcov)) {
    return(NULL)
  }

  # vcov is character
  if (is.character(vcov) && length(vcov) == 1) {

    if (vcov %in% c("classical", "constant", "iid")) {
      return(NULL)
    }

    assert_dependency("sandwich")

    if (vcov == "stata") {
      vcovtype <- "HC1"
    } else if (vcov == "robust") {
      vcovtype <- "HC3"
    } else {
      vcovtype <- vcov
    }

    HC <- c("HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
    if (vcovtype %in% HC) {
      mat <- try(sandwich::vcovHC(model, type = vcovtype, ...), silent = TRUE)
    }

    if (vcovtype == "HAC") {
      mat <- try(sandwich::vcovHAC(model, ...), silent = TRUE)
    }

    if (vcovtype == "NeweyWest") {
      mat <- try(sandwich::NeweyWest(model, ...), silent = TRUE)
    }

    if (vcovtype == "bootstrap") {
      mat <- try(sandwich::vcovBS(model, ...), silent = TRUE)
    }

    if (vcovtype == "Andrews") {
      mat <- try(sandwich::kernHAC(model, ...), silent = TRUE)
    }

    if (vcovtype == "panel-corrected") {
      mat <- try(sandwich::vcovPC(model, ...), silent = TRUE)
    }

    if (vcovtype == "outer-product") {
      mat <- try(sandwich::vcovOPG(model, ...), silent = TRUE)
    }

    if (vcovtype == "weave") {
      mat <- try(sandwich::weave(model, ...), silent = TRUE)
    }

    if (!inherits(mat, "matrix")) {
      msg <- paste0(c("Unable to extract a variance-covariance matrix of type %s from model of class %s. ",
        msg_vcov_error), collapse = "")
      stop(sprintf(msg, vcov, class(model)[1]))
    }
  }

  # vcov is formula (clusters)
  if (isTRUE(checkmate::check_formula(vcov))) {
    assert_dependency("sandwich")
    mat <- try(sandwich::vcovCL(model, cluster = vcov), silent = TRUE)

    if (!inherits(mat, "matrix")) {
      msg <- paste0(c("Unable to extract a clustered variance-covariance matrix from model of class %s. ",
        msg_vcov_error), collapse = "")
      stop(sprintf(msg, class(model)[1]))
    }
  }

  # vcov = function
  if (is.function(vcov)) {
    mat <- try(vcov(model), silent = TRUE)

    # lme4::lmer
    if (inherits(mat, "dpoMatrix")) {
      mat <- as.matrix(mat)
    }

    if (!inherits(mat, "matrix")) {
      msg <- paste0(c("Unable to use the supplied function to extract a variance-covariance matrix from model of class %s. ",
        msg_vcov_error), collapse = "")
      stop(sprintf(msg, class(model)[1]))
    }
  }

  # vcov is matrix
  if (is.matrix(vcov)) {
    mat <- vcov
  }

  # vcov = atomic vector
  if (isTRUE(checkmate::check_atomic_vector(vcov, names = "named"))) {
    out <- data.frame(term = names(vcov), std.error = vcov)

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
      out <- data.frame(term = colnames(mat), std.error = out)
      msg <- paste0(
        "The `lmtest::coeftest` function does not seem to produce a valid result ",
        "when applied to a model of class %s. Only the standard errors have been adjusted, ",
        "but p-values and confidence intervals might not be correct.", collapse = "")
      warning(sprintf(msg, class(model)[1]))
      return(out)
    }
  }

  msg <- paste0(c("Unable to extract a variance-covariance matrix from model of class %s. ",
    msg_vcov_error), collapse = "")
  stop(sprintf(msg, class(model)[1]))
}


get_coeftest <- function(model, vcov, conf_level) {

  if (!isTRUE(check_dependency("lmtest"))) return(NULL)

  gof <- try(
    lmtest::coeftest(model, vcov. = vcov), silent = TRUE)

  gof_ci <- try(
    lmtest::coefci(model, vcov. = vcov, level = conf_level),
    silent = TRUE)

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
      if (v %in% c("robust", "classical", "stata", "classical", "constant",
                   "panel-corrected", "Andrews", "outer-product")) {
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