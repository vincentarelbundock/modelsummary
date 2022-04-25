#' Internal function to subset, rename and re-order gof statistics
#'
#' @keywords internal
map_gof <- function(gof, gof_omit, gof_map) {

  if (is.null(gof) || isTRUE(nrow(gof) == 0)) {
    return(gof)
  }

  # row identifier as first column
  gof$part <- "gof"
  gof <- gof[, unique(c("part", "term", names(gof)))]

  # gof_omit
  if (!is.null(gof_omit)) {
    idx <- !grepl(gof_omit, gof$term, perl = TRUE)
    gof <- gof[idx, , drop = FALSE]
  }


  # map
  gm_raw <- sapply(gof_map, function(x) x$raw)
  gm_clean <- sapply(gof_map, function(x) x$clean)
  gm_omit <- try(sapply(gof_map, function(x) x$omit), silent = TRUE)

  if (is.logical(gm_omit)) {
    if (isTRUE(attr(gof_map, "whitelist"))) {
      gof <- gof[gof$term %in% gm_clean[gm_omit == FALSE], , drop = FALSE]
    } else {
      gof <- gof[!gof$term %in% gm_clean[gm_omit == TRUE], , drop = FALSE]
    }
  } else {
    if (isTRUE(attr(gof_map, "whitelist"))) {
      gof <- gof[gof$term %in% gm_clean, , drop = FALSE]
    }
  }

  tmp <- gm_clean[match(gof$term, gm_raw)]
  tmp[is.na(tmp)] <- gof$term[is.na(tmp)]
  gof$term <- tmp
  idx <- match(gof$term, gm_clean)
  # hack to keep unmatched gof at the end of the table
  # important for unknown glance_custom entries
  idx[is.na(idx)] <- 1e6 + 1:sum(is.na(idx))
  gof <- gof[order(idx, gof$term), ]

  # important for modelsummary_get = "all"
  gof <- unique(gof)

  return(gof)
}

