bind_est_gof <- function(est, gof) {
  if (!inherits(gof, "data.frame") || nrow(gof) == 0) {
    return(est)
  }

  if ("model" %in% colnames(est)) {
    return(est)
  }

  if (!"term" %in% colnames(est)) {
    termcol <- setdiff(colnames(est), "part")[1]
    data.table::setnames(gof, old = "term", new = termcol)
  }

  if (all(colnames(gof) %in% colnames(est))) {
    out <- bind_rows(est, gof)
    return(out)
  }

  # partial matches on model names, but not on known columns
  bad <- c("part", "term", "model", "group", "statistic")
  bad <- stats::na.omit(match(bad, colnames(est)))

  idx <- sapply(colnames(gof), function(x) {
    # first matches
    setdiff(grep(x, colnames(est), fixed = TRUE), bad)[1]
  })
  idx <- stats::na.omit(idx)
  if (length(idx) > 0) {
    data.table::setnames(gof, old = names(idx), new = names(est)[idx])
    out <- bind_rows(est, gof)
  } else {
    out <- est
  }

  return(out)
}
