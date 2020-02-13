#' Extract goodness-of-fit statistics from a single model
#' @param model object type with an available `glance` method.
#' @importFrom broom glance
#' @inheritParams modelsummary
#' @return tibble with goodness-of-fit  statistics
extract_gof <- function(model, fmt = '%.3f', gof_map = NULL) {

    # define gof_map
    if (is.null(gof_map)) {
        gof_map <- modelsummary:::gof_map 
    }

    # extract gof from model object
    gof <- generics::glance(model)

    # extract nobs if not in glance but gof_maps says we want it
    # TODO: This should be fixed upstream in broom
    if ((!'nobs' %in% names(gof)) & ('nobs' %in% gof_map$raw)) { 
        gof$nobs <- tryCatch(stats::nobs(model, use.fallback = TRUE), error = function(e) NULL)
    }

    # subset and re-order gof
    gof_map <- gof_map[!gof_map$omit,]
    idx <- base::intersect(gof_map$raw, colnames(gof))
    gof_map <- gof_map[gof_map$raw %in% idx,]
    gof <- gof[gof_map$raw]

    # round integer/numeric values
    for (i in seq_along(gof)) {
        if (inherits(gof[[i]], 'numeric')) {
            gof[[i]] <- rounding(gof[[i]], gof_map$fmt[i])
        }
    }

    # reshape
    gof <- gof %>%
           tidyr::pivot_longer(cols = 1:ncol(.), names_to = 'term')

    # output
    return(gof)
}
