fixest_multi_names <- function(x) {
    if (requireNamespace("fixest", quietly = TRUE) && utils::packageVersion("fixest") >= "0.10.5") {

        if (settings_equal("function_called", "modelsummary_rbind")) {

          if (!is.null(names(x)) && all(grepl(";", names(x)))) {
            nam <- do.call("rbind", strsplit(names(x), ";"))
            for (i in 1:ncol(nam)) {
              if (anyDuplicated(nam[, i]) == nrow(nam) - 1) {
                nam[, i] <- NA
              }
            }
            out <- stats::na.omit(t(nam))
            out <- apply(out, 2, paste, collapse = "; ")
            out <- trimws(out)
          } else {
            out <- names(x)
          }

        } else {

          fun <- utils::getFromNamespace("models", "fixest")
          tree <- fun(x)
          tree$id <- NULL
          for (i in rev(seq_along(tree))) {
            if (length(unique(tree[[i]])) == 1) {
              tree[[i]] <- NULL
            } else {
              tree[[i]] <- sprintf("%s: %s", names(tree)[i], tree[[i]])
            }
          }
          out <- apply(tree, 1, paste, collapse = "; ")
        }

    } else {
        out <- sprintf("(%s)", seq_along(x))
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
