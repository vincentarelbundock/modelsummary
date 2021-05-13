#' Internal function to prepare stars
#'
#' @noRd
clean_stars <- function(stars) {
  if (is.logical(stars) && isTRUE(stars)) {
    out <- c('+' = .1, '*' = .05, '**' = .01, '***' = 0.001)
  } else if (is.numeric(stars)) {
    out <- sort(stars, decreasing = TRUE)
  } else {
    out <- NULL
  }
  return(out)
}


#' Internal function to prepare stars
#'
#' @noRd
make_stars <- function(pvalues, stars) {
  stars <- clean_stars(stars)
  tmp <- ""
  for (n in names(stars)) {
    tmp <- ifelse(pvalues <= stars[n], n, tmp)
  }
  tmp
}


#' Internal function to prepare stars footnote
#'
#' @noRd
make_stars_note <- function(stars) {
  out <- clean_stars(stars)
  if (!is.null(out)) {
    out <- paste0(names(out), ' p < ', out)
    out <- paste0(out, collapse = ', ')
  }
  return(out)
}
