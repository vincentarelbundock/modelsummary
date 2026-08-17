# Separator used by `data.table::dcast()` below to glue the components of a
# reshaped column name together, as in "model||||statistic". Model labels are
# user-supplied and may contain anything, so code that needs to recover the
# components must split on this string rather than search for substrings.
SHAPE_SEP <- "||||"

# Split reshaped column names into their components.
shape_sep_split <- function(x) {
  strsplit(x, SHAPE_SEP, fixed = TRUE)
}

#' internal function to reshape grouped estimates
#'
#' @keywords internal
#' @noRd
shape_estimates <- function(estimates, shape, conf_level, statistic, estimate) {
  # default
  if (isTRUE(all.equal(shape$shape_formula, term + statistic ~ model))) {
    return(estimates)
  }

  shape_formula <- shape$shape_formula

  idx <- intersect(
    colnames(estimates),
    c("term", "statistic", "group", shape$group_name)
  )

  # long
  out <- data.table::melt(
    data.table::data.table(estimates),
    id.vars = idx,
    variable.name = "model",
    value.name = "estimate"
  )

  if ("statistic" %in% shape$rhs) {
    out$statistic <- rename_statistics(
      out$statistic,
      conf_level = conf_level,
      statistic = statistic,
      estimate = estimate
    )
  }

  # use factors to preserve order in `dcast`
  for (col in c("part", "model", "term", shape$group_name, "statistic")) {
    if (col %in% colnames(out)) {
      out[[col]] <- factor(out[[col]], unique(out[[col]]))
    }
  }

  # wide
  out <- data.table::dcast(
    eval(shape_formula),
    data = out,
    value.var = "estimate",
    sep = SHAPE_SEP
  )

  data.table::setDF(out)

  out[out == "NA"] <- ""
  out[is.na(out)] <- ""

  # empty columns
  idx <- sapply(out, function(x) !all(x == ""))
  out <- out[, idx, drop = FALSE]

  # empty rows
  idx <- setdiff(colnames(out), c("part", "term", "statistic", "model"))
  tmp <- out[, idx, drop = FALSE]
  idx <- apply(tmp, 1, function(x) !all(x == ""))
  out <- out[idx, ]

  return(out)
}
