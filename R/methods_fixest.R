fixest_multi_names <- function(x) {
    if (requireNamespace("fixest", quietly = TRUE) &&
        utils::packageVersion("fixest") >= "0.10.5") {
        fun <- utils::getFromNamespace("models", "fixest")
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



#' @include glance_custom.R
#' @keywords internal
glance_custom_internal.fixest <- function(x, vcov_type = NULL, ...) {
  insight::check_if_installed("fixest", minimum_version = "0.10.4")

  # fixed effects
  out <- data.frame(row.names = "firstrow")
  for (n in x$fixef_vars) {
    out[[paste('FE:', n)]] <- 'X'
  }

  # standard errors
  if (is.null(vcov_type) ||
      vcov_type %in% c("", "Default", "Constant", "IID") ||
      !vcov_type %in% c("vector", "matrix", "function")) {
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
