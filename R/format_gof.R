#' Extract goodness-of-fit statistics from a single model
#'
#' @param model object type with an available `glance` method.
#' @inheritParams modelsummary
#' @return data.frame with goodness-of-fit  statistics
#' @noRd
format_gof <- function(gof, fmt, gof_map, ...) {

  if (!is.data.frame(gof)) {
    return(NULL)
  }

  # convert gof_map to list and vectors
  if (is.null(gof_map)) {
    gm_list <- lapply(1:nrow(modelsummary::gof_map), function(i)
                  as.list(modelsummary::gof_map[i, ]))
    gm_omit <- sapply(gm_list, function(x) x$omit)
  } else if (inherits(gof_map, "data.frame")) {
    gm_list <- lapply(1:nrow(gof_map), function(i)
                  as.list(gof_map[i, ]))
  } else {
    gm_list <- gof_map
  }
  # `as.character` is needed for R-devel changes to `intersect` with empty sets
  gm_raw <- as.character(sapply(gm_list, function(x) x$raw))
  gm_clean <- as.character(sapply(gm_list, function(x) x$clean))

  # round
  unknown <- setdiff(colnames(gof), gm_raw)
  for (u in unknown) {
    gof[[u]] <- rounding(gof[[u]], fmt)
  }
  for (gm in gm_list) {
    if (gm$raw %in% colnames(gof)) {
      gof[[gm$raw]] <- rounding(gof[[gm$raw]], gm$fmt)
    }
  }

  # gof_map = NULL: drop explicit omit
  if (is.null(gof_map)) {
    gof <- gof[, !colnames(gof) %in% gm_raw[gm_omit], drop = FALSE]
  # gof_map != NULL: drop unknown
  } else {
    gof <- gof[, colnames(gof) %in% gm_raw, drop = FALSE]
  }

  # reorder columns
  idx1 <- intersect(gm_raw, colnames(gof))
  idx2 <- setdiff(colnames(gof), gm_raw)
  # avoid errors on some CI platforms
  idx <- unlist(c(idx1, idx2), recursive = TRUE)
  gof <- as.data.frame(gof)
  gof <- gof[, idx, drop = FALSE]

  # some gof were kept
  if (ncol(gof) > 0) {
    # rename
    idx <- match(colnames(gof), gm_raw)
    colnames(gof) <- ifelse(is.na(idx), colnames(gof), gm_clean[idx])

    # reshape
    out <- data.frame(term = names(gof), value = unlist(gof))

  # all gof are excluded return an empty tibble (needs character to match merge type)
  } else {
    out <- data.frame(term = NA_character_, value = NA_character_)
    out <- stats::na.omit(out)
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
