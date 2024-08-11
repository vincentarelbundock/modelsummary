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

  # Remove this code because the new tinytable default uses a better set of options for siunitx by default, which handles *-+() gracefully
  # if (identical(output_format, "latex") && settings_equal("dcolumn_stars_mbox", TRUE)) {
  #   tmp <- sprintf(" \\mbox{%s}", tmp)
  # }

  tmp
}


#' Internal function to prepare stars footnote
#'
#' @noRd
make_stars_note <- function(stars, output_format = NULL) {
  out <- clean_stars(stars)
  if (!is.null(out)) {
    if (identical(output_format, "latex")) {
      out <- paste0(names(out), ' p \\num{< ', out, "}")
    } else {
      out <- paste0(names(out), ' p < ', out)
    }
    out <- paste0(out, collapse = ', ')
  }
  return(out)
}
