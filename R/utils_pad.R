#' pad a character vector with spaces
#' 
#' if there are duplicate or empty entries in a vector, this function adds
#' an arbitrary number of empty spaces to each entry to de-duplicate. this
#' is useful for table-making packages which require unique colnames. they
#' will usually strip white space anyway. This is suboptimal because it
#' makes it harder to select columns, so we only apply pad duplicate
#' entries.
#' @export
#' @keywords internal
pad <- function(x) {
    checkmate::assert_character(x)
    x <- trimws(x)
	dup <- duplicated(x)
    dup[x == ''] <- TRUE
    ndup <- sum(dup)
    if (ndup > 0) {
        pad <- sapply(1:ndup, function(k) strrep(' ', k))
        x[dup] <- paste0(x[dup], pad)
    }
	x
}

