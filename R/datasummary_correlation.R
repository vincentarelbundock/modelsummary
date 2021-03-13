#' Generate a correlation table for all numeric variables in your dataset.
#'
#' @inheritParams datasummary
#' @param method character or function
#' \itemize{
#'   \item character: "pearson", "kendall", "spearman", or "pearspear"
#'     (Pearson correlations above and Spearman correlations below the diagonal)
#'   \item function: takes a data.frame with numeric columns and returns a
#'   square matrix with unique row.names and colnames.
#' }
#' @export
datasummary_correlation <- function(data,
                                    output = 'default',
                                    fmt = 2,
                                    title = NULL,
                                    notes = NULL,
                                    method = "pearson") {

  # sanity checks
  sanity_output(output)

  any_numeric <- any(sapply(data, is.numeric) == TRUE)
  if (any_numeric == FALSE) {
    stop("`datasummary_correlation` can only summarize numeric data columns.")
  }

  checkmate::assert(
    checkmate::check_choice(
      method, 
      c("pearson", "kendall", "spearman", "pearspear")),
    checkmate::check_function(method))

  # assign correlation computation function
  if (is.function(method)) {
    fn <- method
  } else if (method == "pearspear") {
    fn <- correlation_pearspear
  } else {
    fn <- function(x) stats::cor(
      x, 
      use = "pairwise.complete.obs", 
      method = method)
  } 

  # subset numeric and compute correlation
  out <- data[, sapply(data, is.numeric), drop = FALSE]
  out <- fn(out)

  if (!is.matrix(out) ||
      is.null(row.names(out)) ||
      is.null(colnames(out)) ||
      nrow(out) != ncol(out)) {
    stop("The function supplied to the `method` argument did not return a square matrix with row.names and colnames.")
  }

  if (is.character(method) && method != "pearspear") {
    out <- correlation_clean(
      out,
      fmt = fmt,
      triangle = TRUE)
  } else {
    out <- correlation_clean(
      out,
      fmt = fmt,
      triangle = FALSE)
  }

  out <- cbind(rowname = row.names(out), out)

  colnames(out) <- c(' ', out[[1]])

  align <- paste0('l', strrep('r', ncol(out) - 1))

  factory(out,
    align = align,
    hrule = NULL,
    notes = notes,
    output = output,
    title = title)
}

correlation_pearspear <- function(x) {

  pea <- stats::cor(
    x, 
    use = "pairwise.complete.obs", 
    method = "pearson")

  spe <- stats::cor(
    x, 
    use = "pairwise.complete.obs", 
    method = "spearman")

  pea[lower.tri(pea)] <- spe[lower.tri(spe)]

  return(pea)
}

correlation_clean <- function(x, fmt, triangle) {

  out <- data.frame(x)

  for (i in seq_along(out)) {
    out[[i]] <- rounding(out[[i]], fmt)
    out[[i]] <- gsub('0\\.', '\\.', out[[i]])
    out[[i]] <- gsub('1\\.0*', '1', out[[i]])
  }

  if (triangle == TRUE) {
    for (i in 1:nrow(out)) {
      for (j in 1:ncol(out)) {
        out[i, j] <- ifelse(i < j, '.', out[i, j])
      }
    }
  }

  return(out)
}

