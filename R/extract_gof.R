#' Extract goodness-of-fit statistics from a single model
#' @param model object type with an available `glance` method.
#' @importFrom broom glance
#' @importFrom tibble tibble
#' @inheritParams modelsummary
#' @return tibble with goodness-of-fit  statistics
#' @keywords internal
extract_gof <- function(model, fmt, gof_map = NULL) {

    # define gof_map
    if (is.null(gof_map)) {
        gof_map <- modelsummary::gof_map 
    }

    # extract gof from model object
    gof <- generics::glance(model)

    # extract nobs if not in glance but gof_maps says we want it
    # TODO: This should be fixed upstream in broom
    if ((!'nobs' %in% names(gof)) & ('nobs' %in% gof_map$raw)) { 
        gof$nobs <- tryCatch(stats::nobs(model, use.fallback = TRUE), 
                             error = function(e) NULL)
    }

    # drop if gof_map$omiot == TRUE
    bad <- gof_map$raw[gof_map$omit] 
    gof <- gof[setdiff(colnames(gof), bad)] 

    # re-order gof columns
    idx1 <- intersect(gof_map$raw, colnames(gof))
    idx2 <- setdiff(colnames(gof), gof_map$raw)
    gof <- gof[c(idx1, idx2)]

    # if number of gof > 0
    if (ncol(gof) > 0) {

        for (i in seq_along(gof)) {
     
            idx <- match(colnames(gof)[i], gof_map$raw)

            if (!is.na(idx)) { # if gof in gof_map

                # rename
                colnames(gof)[i] <- gof_map$clean[idx]

                # round integer/numeric values
                if (inherits(gof[[i]], 'numeric')) {
                    gof[[i]] <- rounding(gof[[i]], gof_map$fmt[idx])
                } else {	
                    gof[[i]] <- as.character(gof[[i]])	
                }

            } else { # if gof is not in gof_map

                # round integer/numeric values
                if (inherits(gof[[i]], 'numeric')) {
                    gof[[i]] <- rounding(gof[[i]], fmt)
                }
                else {	
                    gof[[i]] <- as.character(gof[[i]])	
                }
            }
        }

        # reshape
        out <- gof %>%
               tidyr::pivot_longer(cols = 1:ncol(.), names_to = 'term')

    } else { # all gof are excluded return an empty tibble (needs character to match merge type)
        out <- tibble::tibble(term = NA_character_, value = NA_character_) %>% tidyr::drop_na()
    }

    # output
    return(out)
}
