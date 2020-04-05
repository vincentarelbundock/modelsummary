#' Internal function to prepare stars
#' 
#' @keywords internal
clean_stars <- function(stars) {
    if (is.logical(stars)) {
        if (stars) {
            stars <- c('*' = .1, '**' = .05, '***' = .01)
        }
    }
    if (is.numeric(stars)) {
        stars <- sort(stars, decreasing = TRUE)
        out <- stars
    } else {
        out <- NULL
    }
    return(out)
}

#' Internal function to prepare stars footnote
#' 
#' @keywords internal
make_stars_note <- function(stars) {
    out <- clean_stars(stars)
    if (!is.null(out)) {
        out <- paste0(names(out), ' p < ', out)
        out <- paste0(out, collapse = ', ')
    }
    return(out)
}
