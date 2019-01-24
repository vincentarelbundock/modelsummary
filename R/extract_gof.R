#' Extract goodness-of-fit statistics from a single model
#'
extract_gof <- function(model, fmt = '%.3f', gof_map = NULL, ...) {
    if (is.null(gof_map)) {
        gof_map <- gtsummary::gof_map
    }
    # extract gof from model object
    gof <- broom::glance(model)

    # extract nobs if not available from glance
    # TODO: This should be fixed upstream
    if (!'n' %in% names(gof)) {
        gof$n <- tryCatch(stats::nobs(model), error = function(e) NULL)
    }

    # round numeric values and rename
    for (column in colnames(gof)) {
        # is gof in gof_map?
        idx <- match(column, gof_map$raw)
        if (!is.na(idx)) { # yes
            if (class(gof[[column]]) %in% c('numeric', 'integer')) {
                gof[[column]] <- rounding(gof[[column]], gof_map$fmt[idx])
            } else {
                gof[[column]] <- as.character(gof[[column]])
            }
            colnames(gof)[colnames(gof) == column] <- gof_map$clean[idx]
        } else { # no
            if (class(gof[[column]]) %in% c('numeric', 'integer')) {
                gof[[column]] <- rounding(gof[[column]], fmt)
            } else {
                gof[[column]] <- as.character(gof[[column]])
            }
        }
    }
    # reshape
    gof <- gof %>%
           tidyr::gather(term, value)
    # output
    return(gof)
}
