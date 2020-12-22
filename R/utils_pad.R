#' pad a character vector with spaces
#'
#' if there are duplicate or empty entries in a vector, this function adds
#' an arbitrary number of empty spaces to each entry to de-duplicate. this
#' is useful for table-making packages which require unique colnames. they
#' will usually strip white space anyway. This is suboptimal because it
#' makes it harder to select columns, so we only apply pad duplicate
#' entries.
#' @noRd
pad <- function(x) {
  checkmate::assert_character(x)
  out <- trimws(x)
  out[out == ''] <- ' '
  tab <- table(out)
  for (i in seq_along(tab)) {
    if (tab[i] > 1) {
      idx <- which(out == names(tab)[i])
      tmp <- sapply(0:(length(idx) - 1), function(k)
        paste0(names(tab)[i], strrep(' ', k)))
      out[idx] <- tmp
    }
  }
  return(out)
}
