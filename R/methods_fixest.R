fixest_multi_names <- function(x) {
    if (requireNamespace("fixest", quietly = TRUE) &&
        utils::packageVersion("fixest") > "0.10.5") {
        fun <- utils::getFromNamespace("model", "fixest")
        tree <- fun(x)
        if ("lhs" %in% names(tree)) {
            out <- tree$lhs
        } else if ("sample" %in% names(tree)) {
            out <- sprintf("%s: %s", tree$sample.var, tree$sample)
        } else {
            out <- paste("Model", seq_along(x))
        }
    } else {
        out <- paste("Model", seq_along(x))
    }
    return(out)
}


#' @include get_vcov.R
get_vcov.fixest <- function(model, vcov = NULL, conf_level = NULL, ...) {

  ## Versions older than 0.10.0 require a different approach, since they don't
  ## support the flexible vcov argument
  insight::check_if_installed("fixest", minimum_version = "0.10.4")

  ## known fixest vcovs
  fixest_vcovs <- c("iid", "standard", "hetero", "HC1", "White", "cluster",
                    "twoway", "NW", "newey_west", "DK", "driscoll_kraay",
                    "conley")
  ## Equivalent aliases used by modelsummary (excl. those common to both)
  fixest_vcov_aliases <- c("iid" = "classical",
                           "iid" = "constant",
                           "HC1" = "stata",
                           "NW" = "NeweyWest")

  is_func <- is.function(vcov)
  is_mat <- is.matrix(vcov)
  is_form <- inherits(vcov, "formula")

  if (!is_func && !is_form && !is_mat && vcov %in% fixest_vcov_aliases) {
    vcov <- names(fixest_vcov_aliases)[which(fixest_vcov_aliases %in% vcov)]
  }

  ## if a known or compatible fixest vcov argument, use the dedicated
  ## fixest.vcov method
  if (is.null(vcov) || is_func || is_form || is_mat || vcov %in% fixest_vcovs) {

    mat <- vcov(model, vcov = vcov)

    # otherwise get_coeftest returns NULL and std.errors are not adjusted
    if (is.matrix(mat)) {
      insight::check_if_installed("lmtest")
    }

    out <- get_coeftest(model, mat, conf_level)
    return(out)

    ## else coerce to iid error for sandwich adjustment below
  } else {
    model <- summary(model, vcov = "iid")
  }

  get_vcov.default(model = model, vcov = vcov, conf_level = conf_level)
}

get_vcov.fixest_multi <- get_vcov.fixest



#' @include glance_custom.R
#' @keywords internal
glance_custom_internal.fixest <- function(x, vcov_type = NULL, ...) {
  insight::check_if_installed("fixest", minimum_version = "0.10.4")

  out <- data.frame(row.names = "firstrow")
  for (n in x$fixef_vars) {
    out[[paste('FE:', n)]] <- 'X'
  }
  if (is.null(vcov_type) || !vcov_type %in% c("vector", "matrix", "function")) {
    fvcov_type <- attr(summary(x)[["se"]], "type")
    if (isTRUE(grepl("^Clustered", fvcov_type))) {
      fvcov_type <- gsub("^Clustered \\((.*)\\)$", "by: \\1", fvcov_type)
    }
    out[["vcov.type"]] <- fvcov_type
  }
  row.names(out) <- NULL
  return(out)
}

glance_custom_internal.multi <- glance_custom_internal.fixest
