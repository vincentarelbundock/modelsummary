#' Execute code silently
#'
#' @export
#' @keywords internal
hush <- function(code) {
    void <- utils::capture.output({
        out <- invisible(suppressMessages(suppressWarnings(code)))
    })
    return(out)
}
