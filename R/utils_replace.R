#' replace character vector entries using a named vector dictionary
#' @noRd
#' 
replace_dict <- function(x, dict, interaction = TRUE) {
  # this is simpler and faster, but does not handle interactions
  # for (i in seq_along(dict)) {
  #   x[x == names(dict)[i]] <- dict[i]
  # }
  for (i in seq_along(dict)) {
    x <- gsub(sprintf("^%s$", names(dict)[i]), dict[i], x)
    x <- gsub(sprintf(":%s$", names(dict)[i]), paste0(":", dict[i]), x)
    x <- gsub(sprintf("^%s:", names(dict)[i]), paste0(dict[i], ":"), x)
    x <- gsub(sprintf(":%s:", names(dict)[i]), paste0(":", dict[i], ":"), x)
  }

  return(x)
}
