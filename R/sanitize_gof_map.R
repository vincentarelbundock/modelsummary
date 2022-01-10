#' sanity check
#'
#' @noRd
sanitize_gof_map <- function(gof_map) {

  # sanity checks
  checkmate::assert(
    checkmate::check_data_frame(gof_map, null.ok = TRUE, min.cols = 3),
    checkmate::check_list(gof_map, null.ok = TRUE),
    combine = "or")
  if (inherits(gof_map, "data.frame")) {
    checkmate::assert_true(all(c("raw", "clean", "fmt") %in% colnames(gof_map)))
  } else if (class(gof_map)[1] == "list") {
    for (gm in gof_map) {
      checkmate::assert_list(gm, len = 3, names = "named")
      checkmate::assert_true(all(c("raw", "clean", "fmt") %in% names(gm)))
    }
  }

  # default map
  if (is.null(gof_map)) {
    gof_map <- modelsummary::gof_map
    whitelist <- FALSE
  } else {
    whitelist <- TRUE
  }

  # gof_map must be a list of lists because `fmt` can be a function
  if (class(gof_map)[1] == "list") {
    out <- gof_map
  } else if (inherits(gof_map, "data.frame")) {
    # list column can include functions
    if (class(gof_map$fmt)[1] == "list") {
        out <- lapply(1:nrow(gof_map), function(i) unlist(gof_map[i, , drop = FALSE]))
    } else {
        out <- lapply(1:nrow(gof_map), function(i) as.list(gof_map[i, , drop = FALSE]))
    }
  }

  attr(out, "whitelist") <- whitelist

  return(out)
}
