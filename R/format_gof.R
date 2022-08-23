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

  # `as.character` is needed for R-devel changes to `intersect` with empty sets
  gm_raw <- as.character(sapply(gof_map, function(x) x$raw))
  gm_clean <- as.character(sapply(gof_map, function(x) x$clean))

  # formating arguments priority: `fmt` > `gof_map` > 3
  # rounding() escapes the GOF values. we escape the GOF names at the end
  for (g in gof_map) {
    if (!g$raw %in% names(fmt)) {
      fmt[[g$raw]] <- g$fmt
    }
  }
  for (g in colnames(gof)) {
    if (g %in% names(fmt)) {
      gof[[g]] <- rounding(gof[[g]], fmt[[g]])
    } else {
      # set by default in `sanitize_fmt()`
      gof[[g]] <- rounding(gof[[g]], fmt[["fmt"]])
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

  # factor to character (otherwise gof_map breaks under R < 4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }

  if (settings_get("escape")) {
    out$term <- escape_string(out$term)
  }

  # output
  row.names(out) <- NULL
  return(out)
}
