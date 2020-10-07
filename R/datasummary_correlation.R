#' Generate a correlation table for all numeric variables in your dataset.
#'
#' @inheritParams datasummary
#' @export
datasummary_correlation <- function(data,
                                    output = 'default',
                                    fmt = 2,
                                    title = NULL,
                                    notes = NULL) {

  sanity_output(output)

  clean_r <- function(x) {
    x <- rounding(x, fmt)
    x <- gsub('0\\.', '\\.', x)
    x <- gsub('1\\.00', '1', x)
    return(x)
  }

  nvar <- ncol(data)
  out <- data %>%
    dplyr::select(where(is.numeric)) %>%
    stats::cor(use = 'pairwise.complete.obs') %>%
    data.frame %>%
    cbind(rowname=row.names(.), .) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), clean_r))

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
