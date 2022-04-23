#' sanity check
#'
#' @noRd
sanitize_group <- function(group) {

  checkmate::assert_class(group, "formula")

  rhs <- all.vars(stats::update(group, "0 ~ ."))
  lhs <- all.vars(stats::update(group, ". ~ 0"))
  variables <- c(rhs, lhs)

  if (!all(c("model", "term") %in% c(lhs, rhs))) {
    flag_error <- TRUE
  }

  group_name <- setdiff(variables, c("term", "model", "statistic"))
  if (length(group_name) > 1) {
    stop('The `group` formula can only include one group name. The other terms must be: "term", "model", or "statistic".')
  }

  if (length(group_name) == 0) {
    group_name <- NULL
  }

  group_formula <- group
  if (!"statistic" %in% variables) {
    group_formula <- update(group_formula, ". + statistic ~ .")
  }

  out <- list("lhs" = lhs,
              "rhs" = rhs,
              "group_name" = group_name,
              "group_formula" = group_formula)

  return(out)
}
