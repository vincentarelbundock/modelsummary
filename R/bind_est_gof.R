bind_est_gof <- function(est, gof) {
  if (!inherits(gof, "data.frame") || nrow(gof) == 0) {
    return(est)
  }

  if (all(colnames(gof) %in% colnames(est))) {
    out <- bind_rows(est, gof)
    return(out)
  }

  # partial matches on model names
  idx <- sapply(colnames(gof), function(x) grep(x, colnames(est))[1]) # first matches
  if (any(is.na(idx))) {
    return(est)
  }

  colnames(gof) <- colnames(est)[idx]
  out <- bind_rows(est, gof)
  return(out)
}

