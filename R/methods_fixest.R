#' @include get_vcov.R
get_vcov.fixest <- function(model, vcov = NULL, conf_level = NULL, ...) {

  ## Versions older than 0.10.0 require a different approach, since they don't
  ## support the flexible vcov argument
  fixest_0_10 <- utils::packageVersion("fixest") >= "0.10.0"

  ## Match supported character args based on version
  if (fixest_0_10) {
    ## known fixest vcovs
    fixest_vcovs <- c("iid", "standard", "hetero", "HC1", "White", "cluster",
                      "twoway", "NW", "newey_west", "DK", "driscoll_kraay",
                      "conley")
    ## Equivalent aliases used by modelsummary (excl. those common to both)
    fixest_vcov_aliases <- c("iid" = "classical",
                             "iid" = "constant",
                             "HC1" = "stata",
                             "NW" = "NeweyWest")
  } else {
    ## known fixest vcovs
    fixest_vcovs <- c("standard", "hetero", "cluster", "twoway", "threeway", "fourway")
    ## Equivalent aliases used by modelsummary  (excl. those common to both)
    fixest_vcov_aliases <- c("standard" = "iid",
                             "standard" = "classical",
                             "standard" = "constant",
                             "hetero" = "stata")
  }

  is_func <- is.function(vcov)
  is_mat <- is.matrix(vcov)
  is_form <- inherits(vcov, "formula")

  if (!is_func && !is_form && !is_mat && vcov %in% fixest_vcov_aliases) {
    vcov <- names(fixest_vcov_aliases)[which(fixest_vcov_aliases %in% vcov)]
  }

  ## if a known or compatible fixest vcov argument, use the dedicated
  ## fixest.vcov method
  if (fixest_0_10) {
    ## if a known or compatible fixest vcov argument, use the dedicated
    ## fixest.vcov method
    if (is.null(vcov) || is_func || is_form || is_mat || vcov %in% fixest_vcovs) {
      mat <- vcov(model, vcov = vcov)
      out <- get_coeftest(model, mat, conf_level)
      return(out)
      ## else coerce to iid error for sandwich adjustment below
    } else {
      model <- summary(model, vcov = "iid")
    }
    ## fixest versions older than 0.10.0 are less flexible
  } else {
    if (is_form) {
      mat <- vcov(summary(model, cluster = vcov))
      out <- get_coeftest(model, mat, conf_level)
      return(out)
    } else if (is_mat) {
      out <- get_coeftest(model, mat, conf_level)
      return(out)
    } else if (vcov %in% fixest_vcovs) {
      mat <- vcov(summary(model, se = vcov))
      out <- get_coeftest(model, mat, conf_level)
      return(out)
    } else {
      model <- summary(model, se = "standard")
    }
  }

  get_vcov.default(model = model, vcov = vcov, conf_level = conf_level)
}

get_vcov.fixest_multi <- get_vcov.fixest



#' @include glance_custom.R
#' @keywords internal
glance_custom_internal.fixest <- function(x, vcov_type = NULL, ...) {
  assert_dependency("fixest")
  out <- data.frame(row.names = "firstrow")
  for (n in x$fixef_vars) {
    out[[paste('FE:', n)]] <- 'X'
  }
  if (is.null(vcov_type) || !vcov_type %in% c("vector", "matrix", "function")) {
    fvcov_type <- attr(fixest::coeftable(x), "type")
    if (utils::packageVersion("fixest") >= "0.10.0") {
      if (isTRUE(grepl("^Clustered", fvcov_type))) {
        fvcov_type <- gsub("^Clustered \\((.*)\\)$", "by: \\1", fvcov_type)
      }
    } else {
      if (isTRUE(grepl("^Clustered", fvcov_type))) {
        fvcov_type <- gsub("^Clustered \\((.*)\\)$", "by: \\1", fvcov_type)
      } else {
        fvcov_type <- gsub("^Two-way |^Three-way |^Four-way ", "by: ", fvcov_type)
        fvcov_type <- gsub("\\(|\\)", "", fvcov_type)
      }
    }
    out[["vcov.type"]] <- fvcov_type
  }
  row.names(out) <- NULL
  return(out)
}

glance_custom_internal.multi <- glance_custom_internal.fixest
