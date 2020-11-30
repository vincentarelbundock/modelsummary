#' replace character vector entries using a named vector dictionary
#' 
#' @keywords internal
replace_dict <- function(x, dict) {
  for (i in seq_along(dict)) {
    x[x == names(dict)[i]] <- dict[i]
  }
  return(x)
}
