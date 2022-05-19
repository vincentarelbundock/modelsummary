#' sanity check
#'
#' @noRd
sanitize_fmt <- function(fmt, calling_function = NULL) {

    # modelsummary uses a named list, but not datasummary
    if (isTRUE(calling_function == "modelsummary")) {
        checkmate::assert(
            checkmate::check_character(fmt, len = 1, null.ok = TRUE),
            checkmate::check_numeric(fmt, len = 1, lower = 0),
            checkmate::check_function(fmt),
            checkmate::check_list(fmt, names = "unique"))

        if (!isTRUE(checkmate::check_list(fmt))) {
            fmtlist <- list("fmt" = fmt)
            if ("conf.int" %in% names(fmtlist) && !"conf.low" %in% names(fmtlist)) {
                fmtlist[["conf.low"]] <- fmtlist[["conf.high"]] <- fmtlist[["conf.int"]]
            }
            fmt <- fmtlist
        } else {
            if (!"fmt" %in% names(fmt)) {
                fmt[["fmt"]] <- 3
            }
        }

    } else {
        checkmate::assert(
            checkmate::check_character(fmt, len = 1, null.ok = TRUE),
            checkmate::check_numeric(fmt, len = 1, lower = 0),
            checkmate::check_function(fmt))
    }

    return(fmt)
}
