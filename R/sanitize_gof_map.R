#' sanity check
#'
#' @noRd
sanitize_gof_map <- function(gof_map) {
  # sanity checks
  checkmate::assert(
    checkmate::check_character(gof_map, null.ok = TRUE, min.len = 1),
    checkmate::check_data_frame(gof_map, null.ok = TRUE, min.cols = 3),
    checkmate::check_list(gof_map, null.ok = TRUE),
    combine = "or"
  )

  # omit everything
  if (isTRUE(is.na(gof_map)) || isTRUE(gof_map == "none")) {
    return(NA)
  }

  # character vector
  # min.len = 1, otherwise list() triggers, and we want list() to omit all
  if (
    !isTRUE(gof_map == "all") && # handle this case later
      isTRUE(checkmate::check_character(gof_map, min.len = 1))
  ) {
    known <- intersect(gof_map, modelsummary::gof_map$raw)
    unknown <- setdiff(gof_map, modelsummary::gof_map$raw)
    if (length(known) > 0) {
      idx <- match(known, modelsummary::gof_map$raw)
      known <- modelsummary::gof_map[idx, ]
    } else {
      known <- NULL
    }
    if (length(unknown) > 0) {
      unknown <- data.frame(raw = unknown, clean = unknown, fmt = 0)
    } else {
      unknown <- NULL
    }
    gof_map <- bind_rows(known, unknown)

    # continuous integration weird issue
    if (inherits(gof_map, "data.frame") && nrow(gof_map) > 0) {
      gof_map[["omit"]] <- FALSE
    }
  }

  # data.frame
  if (inherits(gof_map, "data.frame")) {
    checkmate::assert_true(all(c("raw", "clean", "fmt") %in% colnames(gof_map)))
  } else if (class(gof_map)[1] == "list") {
    for (gm in gof_map) {
      checkmate::assert_list(gm, len = 3, names = "named")
      checkmate::assert_true(all(c("raw", "clean", "fmt") %in% names(gm)))
    }
  }

  if (isTRUE(gof_map == "all")) {
    gof_map <- modelsummary::gof_map
    gof_map$omit <- FALSE
    whitelist <- FALSE
  } else if (is.null(gof_map)) {
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
      out <- lapply(
        1:nrow(gof_map),
        function(i) unlist(gof_map[i, , drop = FALSE])
      )
    } else {
      out <- lapply(
        1:nrow(gof_map),
        function(i) as.list(gof_map[i, , drop = FALSE])
      )
    }
  }

  attr(out, "whitelist") <- whitelist

  return(out)
}
