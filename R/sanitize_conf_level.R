#' sanity check
#'
#' @noRd
sanity_conf_level_modelplot <- function(conf_level) {
  flag <- checkmate::check_number(
    conf_level,
    lower = 0,
    upper = .999999999999,
    null.ok = TRUE
  )
  if (!isTRUE(flag)) {
    msg <- format_msg(
      "The `conf_level` argument must be a number between 0 and 1. Type `?modelplot`
        for details. This error is sometimes raised when users supply multiple models
        to `modelplot` but forget to wrap them in a list. This works:
        `modelplot(list(model1, model2))`. This does *not* work: `modelplot(model1,
        model2)`"
    )
    stop(msg, call. = FALSE)
  }
}


#' sanity check
#'
#' @noRd
sanitize_conf_level <- function(conf_level, estimate, statistic) {
  checkmate::assert_number(
    conf_level,
    lower = 0,
    upper = .999999999999,
    null.ok = TRUE
  )
  # faster to omit CI when we don't need it
  if (any(grepl("conf", unlist(c(estimate, statistic))))) {
    return(conf_level)
  } else {
    return(NULL)
  }
}
