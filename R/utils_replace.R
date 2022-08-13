#' replace character vector entries using a named vector dictionary
#' @noRd
#' 
replace_dict <- function(x, dict, interaction = FALSE) {
  for (i in seq_along(dict)) {
    x[x == names(dict)[i]] <- dict[i]
    if (interaction) {
      # can't use regex because of "(Intercept)" and similar user-defined arbitrary strings
      x <- gsub(sprintf(":%s:", names(dict)[i]), paste0(":", dict[i], ":"), x, fixed = TRUE)
      x <- gsub(sprintf(":%s", names(dict)[i]), paste0(":", dict[i]), x, fixed = TRUE)
      x <- gsub(sprintf("%s:", names(dict)[i]), paste0(dict[i], ":"), x, fixed = TRUE)
    }
  }

  return(x)
}
