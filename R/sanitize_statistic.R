#' sanity_check
#'
#' @noRd
sanitize_statistic <- function(statistic, shape, conf_level) {
  checkmate::assert_character(statistic, null.ok = TRUE)

  if (is.null(conf_level) && any(grepl("conf", statistic))) {
    msg <- "Cannot display a confidence interval when `conf_level=NULL`."
    stop(msg, call. = FALSE)
  }

  if ("statistic" %in% shape$rhs && "conf.int" %in% statistic) {
    idx <- grep("conf.int", statistic)
    statistic[idx] <- "conf.low"
    # conf.int in last position
    if (idx == length(statistic)) {
      statistic <- c(statistic, "conf.high")
    } else {
      statistic <- c(
        statistic[1:idx],
        "conf.high",
        statistic[(idx + 1):length(statistic)]
      )
    }
  }
  return(statistic)
}
