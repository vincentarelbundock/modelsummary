#' @include glance_custom.R
#' @keywords internal
glance_custom_internal.lm_robust <- function(x, vcov_type = NULL, ...) {
    insight::check_if_installed("estimatr")
    out <- data.frame(row.names = "firstrow")
    if (is.null(vcov_type) || !vcov_type %in% c("vector", "matrix", "function")) {
        if (x$clustered) {
            # use `se_type` otherwise displayed on separate rows from `vcov.type`
            out[["se_type"]] <- paste("by:", x$call$clusters)
        }
    }
    row.names(out) <- NULL
    return(out)
}


#' @keywords internal
glance_custom_internal.iv_robust <- glance_custom_internal.lm_robust
