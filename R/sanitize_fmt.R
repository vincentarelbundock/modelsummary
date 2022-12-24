#' sanity check
#'
#' @noRd
sanitize_fmt <- function(fmt, ...) {
    checkmate::assert(
        checkmate::check_character(fmt, len = 1, null.ok = TRUE),
        checkmate::check_numeric(fmt, len = 1, lower = 0),
        checkmate::check_function(fmt),
        checkmate::check_list(fmt, names = "unique"),
        checkmate::check_class(fmt, "fmt_factory"))

    if (inherits(fmt, "fmt_factory")) {
        out <- fmt
    } else if (isTRUE(checkmate::check_numeric(fmt))) {
        out <- fmt_decimal(digits = fmt)
    } else if (isTRUE(checkmate::check_function(fmt))) {
        out <- fmt_function(fmt)
    } else {
        out <- fmt_identity()
    }
    return(out)
}
