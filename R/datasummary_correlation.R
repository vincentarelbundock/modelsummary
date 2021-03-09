#' Generate a correlation table for all numeric variables in your dataset.
#'
#' @inheritParams datasummary
#' @param method character or function
#' \itemize{
#'   \item character: "pearson", "kendall", or "spearman"
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
    checkmate::check_choice(method, c("pearson", "kendall", "spearman")),
    checkmate::check_function(method))

  if (is.character(method)) {
    fn = function(x) stats::cor(x, use = "pairwise.complete.obs", method = method)
  } else {
    fn = method
  }

  # subset numeric and compute correlation
  out <- data[, sapply(data, is.numeric), drop = FALSE]
  out <- fn(out)

  if (is.function(method) && !is.matrix(out)) {
    stop("The function supplied to the `method` argument did not return a square matrix with row.names and colnames.")
  }

  out <- data.frame(out)
  out <- cbind(rowname=row.names(out), out)

  clean_r <- function(x) {
    x <- rounding(x, fmt)
    x <- gsub('0\\.', '\\.', x)
    x <- gsub('1\\.0*', '1', x)
    return(x)
  }

  for (i in seq_along(out)) {
    if (is.numeric(out[[i]])) {
      out[[i]] <- clean_r(out[[i]])
    }
  }

  for (i in 1:nrow(out)) {
    for (j in 2:ncol(out)) {
      out[i, j] <- ifelse(i + 1 < j, '.', out[i, j])
    }
  }

  colnames(out) <- c(' ', out[[1]])

  align <- paste0('l', strrep('r', ncol(out) - 1))

  factory(out,
    align = align,
    hrule = NULL,
    notes = notes,
    output = output,
    title = title)

}
