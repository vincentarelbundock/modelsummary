#' @include glance_custom.R
#' @keywords internal
glance_custom_internal.MP <- function(x, vcov_type = NULL, ...) {
  assert_dependency("did")
  out <- data.frame(row.names = "firstrow")
  if (is.null(vcov_type) || !vcov_type %in% c("vector", "matrix", "function")) {
    if (x$DIDparams$bstrap) {
      if (!is.null(x$DIDparams$clustervars)) {
        cluster_vars = paste(x$DIDparams$clustervars, collapse = " & ")
      } else {
        cluster_vars = x$DIDparams$idname
      }
      out[['vcov.type']] <- paste("by:", cluster_vars)
    }
  }
  row.names(out) <- NULL
  return(out)
}


#' @keywords internal
glance_custom_internal.AGGTEobj <- glance_custom_internal.MP
