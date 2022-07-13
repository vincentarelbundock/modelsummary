#' @export
#' @noRd
hush <- function(code) {
    void <- capture.output({
        out <- invisible(suppressMessages(suppressWarnings(code)))
    })
    return(out)
}
