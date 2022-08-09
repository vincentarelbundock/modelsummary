#' Extract labels of variables used in a model
#'
#' @return A list with two elements:
#' * "old" is a character vector containing the variables used that have a label
#'   in the data;
#' * "new" is a character vector containing the labels of the variables in "old".

get_labs <- function(mod) {
  tmp <- try(insight::get_data(mod), silent = TRUE)
  lab <- sapply(tmp, is_labelled)
  old <- names(tmp[lab])
  new <- c()
  for (i in seq_along(old)) {
    new[i] <- get_label(tmp[[old[i]]])
  }
  names(new) <- NULL
  return(list(old = old, new = new))
}


#' Internal function used in `datasummary()`.
#' It replaces variable names by variable labels, if they exist.
#' @param dse Table created via `datasummary_extract()`.
#' @param data Original dataset on which `datasummary()` was applied.
#' @noRd

apply_label <- function(dse, data) {

  row_names <- dse[, 1]
  labelled_rownames <- unlist(lapply(row_names, function(x) {
    is_labelled(data[[x]])
  }))
  labelled_rownames <- which(labelled_rownames)

  if (length(labelled_rownames) > 0) {
    new_row_names <- row_names
    for (i in labelled_rownames) {
      new_row_names[i] <- get_label(data[[row_names[i]]])
    }
    dse[, 1] <- new_row_names
  }

  dse

}

# taken from sjlabelled::is_labelled
is_labelled <- function (x) {
  inherits(x, c("labelled", "haven_labelled"))
}

# adapted from sjlabelled::get_label
get_label <- function(x) {
  return(attr(x, "label", exact = TRUE))
}
