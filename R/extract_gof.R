#' Extract goodness-of-fit statistics from a single model
#' @param model object type with an available `glance` method.
#' @importFrom broom glance
#' @inheritParams modelsummary
#' @return data.frame with goodness-of-fit  statistics
#' @keywords internal
extract_gof <- function(model, fmt, gof_map, ...) {

  # define gof_map if not supplied
  if (is.null(gof_map)) {
    gof_map <- modelsummary::gof_map
    if (isTRUE(inherits(model, "lm"))) {
      gof_map$clean[gof_map$raw == "statistic"] <- "F"
      gof_map$omit[gof_map$raw == "statistic"] <- FALSE
    }
  }

  # extract gof from model object
  gof <- generics::glance(model, ...)

  # glance_custom
  gof_custom <- glance_custom(model)
  sanity_gof(gof, gof_custom)
  if (!is.null(gof_custom)) {
    for (n in colnames(gof_custom)) {
      gof[[n]] <- gof_custom[[n]]
    }
  }

  # drop if gof_map$omit == TRUE
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
    out <- data.frame(term = names(gof), value = unlist(gof))

  } else { # all gof are excluded return an empty tibble (needs character to match merge type)
    out <- data.frame(term = NA_character_, value = NA_character_) %>%
      stats::na.omit()
  }

  # factor to character (otherwise gof_map breaks under R < 4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }

  # output
  return(out)
}
