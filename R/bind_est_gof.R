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

  # Match GOF columns to the estimate columns of the same model. A reshaped
  # estimate column name glues several components together, as in
  # "model||||statistic", and a model label may itself be or contain a
  # structural name such as "term". So compare whole components rather than
  # searching for substrings, and consider every component rather than
  # assuming the model label comes first: the component order follows the
  # `shape` formula.
  structural <- c("part", "term", "model", "group", "statistic")
  bad <- stats::na.omit(match(structural, colnames(est)))

  est_components <- shape_sep_split(colnames(est))
  candidates <- setdiff(colnames(gof), structural)

  idx <- sapply(candidates, function(x) {
    hit <- which(vapply(est_components, function(k) x %in% k, logical(1)))
    setdiff(hit, bad)[1]
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
