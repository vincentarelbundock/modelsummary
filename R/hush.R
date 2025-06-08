#' Execute code silently
#'
#' @keywords internal
hush <- function(code) {
  void <- utils::capture.output({
    out <- invisible(
      suppressMessages(
        suppressWarnings(
          tryCatch(code, error = function(e) NULL)
        )
      )
    )
  })
  return(out)
}
