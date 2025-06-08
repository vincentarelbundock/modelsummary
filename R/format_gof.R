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

  # factor to character (otherwise gof_map breaks under R < 4.0.0)
  for (i in seq_along(gof)) {
    if (is.factor(gof[[i]])) {
      gof[[i]] <- as.character(gof[[i]])
    }
  }

  # `as.character` is needed for R-devel changes to `intersect` with empty sets
  gm_raw <- as.character(sapply(gof_map, function(x) x$raw))
  gm_clean <- as.character(sapply(gof_map, function(x) x$clean))

  # formating arguments priority: `fmt` > `gof_map` > 3
  if (inherits(fmt, "fmt_statistic")) {
    gof <- fmt(gof, unknown = FALSE)
  }

  for (g in gof_map) {
    if (is.numeric(gof[[g$raw]])) {
      if (g$raw %in% colnames(gof)) {
        fun <- sanitize_fmt(g$fmt)
        gof[[g$raw]] <- fun(gof[[g$raw]])
      } else {
        fun <- sanitize_fmt(fmt)
        gof[[g$raw]] <- fmt(gof[[g$raw]])
      }
    }
  }

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

  # output
  row.names(out) <- NULL
  return(out)
}
