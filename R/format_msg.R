format_msg <- function(x) {
  x <- strsplit(x, split = "\n")[[1]]
  x <- trimws(x)
  x <- paste0("\n", paste(x, collapse = "\n"), "\n")
  return(x)
}
