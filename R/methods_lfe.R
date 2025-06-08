#' @include glance_custom.R
#' @keywords internal
glance_custom_internal.felm <- function(x, vcov_type = NULL, ...) {
  insight::check_if_installed("lfe")
  out <- data.frame(row.names = "firstrow")
  if (is.null(vcov_type) || !vcov_type %in% c("vector", "matrix", "function")) {
    if (!is.null(x$clustervar)) {
      cluster_vars <- paste(names(x$clustervar), collapse = " & ")
      out[['vcov.type']] <- paste("by:", cluster_vars)
    }
  }
  row.names(out) <- NULL
  return(out)
}
