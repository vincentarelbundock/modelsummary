#' internal function
#'
#' @param v a string or formula describing the standard error type
#' @keywords internal
get_vcov_type <- function(v) {
    if (is.character(v)) {
        if (v %in% c("robust", "classical", "stata", "classical", "constant")) {
            out <- tools::toTitleCase(v)
        } else {
            out <- toupper(v)
        }
    } else if (inherits(v, "formula")) {
        out <- paste("C:", as.character(v)[2])
    } else {
        out <- ""
    }
    return(out)
}

