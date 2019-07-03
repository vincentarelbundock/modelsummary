#' Extract goodness-of-fit statistics from a single model
#' @param model object type with an available `glance` method.
#' @importFrom broom glance
#' @inheritParams modelsummary
#' @return tibble with goodness-of-fit  statistics
extract_gof <- function(model, fmt = '%.3f', gof_map = NULL) {

    # extract gof from model object
    gof <- generics::glance(model)

    # extract nobs if not available from glance
    # TODO: This should be fixed upstream in broom
    if (!'nobs' %in% names(gof)) {
        gof$nobs <- tryCatch(stats::nobs(model), error = function(e) NULL)
    }

    # round integer/numeric values
    fmt_gof <- gof_map$fmt[match(names(gof), gof_map$raw)]
    fmt_gof[is.na(fmt_gof)] <- fmt
    for (i in seq_along(gof)) {
        if (class(gof[[i]]) %in% c('numeric', 'integer')) {
            gof[[i]] <- rounding(gof[[i]], fmt_gof[i])
        } else {
            gof[[i]] <- as.character(gof[[i]])
        }
    }

    # reshape
    gof <- gof %>%
           tidyr::gather(term, value)

    # output
    return(gof)
}
