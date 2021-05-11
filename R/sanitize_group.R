#' sanity check
#'
#' @noRd
sanitize_group <- function(group) {

  flag_error <- FALSE

  checkmate::assert_formula(group)

  rhs <- all.vars(stats::update(group, "0 ~ ."))
  lhs <- all.vars(stats::update(group, ". ~ 0"))
  variables <- c(rhs, lhs)

  if (length(intersect(rhs, lhs) > 0)) {
    stop("The `group` formula cannot include the same variable on both sides.")
  }

  if (!all(c("model", "term") %in% c(lhs, rhs))) {
    flag_error <- TRUE
  }

  if (length(variables) != length(unique(variables))) {
    flag_error <- TRUE
  }

  if (length(variables) > 3) {
    flag_error <- TRUE
  } else if (length(variables) == 2) {
    group_name <- NULL
  } else {
    group_name <- setdiff(c(lhs, rhs), c("term", "model"))
  }

  if (flag_error == TRUE) {
    stop('The `group` argument must be a two-sided formula with two or three components. The formula must include a component named "term", which represents the parameters of the model. The formula must include a component named "model", which represents the different models being summarized. For example,

model ~ term

displays models as rows and parameter estimates as columns. Inverting the formula would display models as columns and terms as rows.

The formula can also include a third, optional, component: a group identifier. In contrast to the "term" and "model" components, the name of the group identifier is not fixed. It must correspond to the name of a column in the data.frame produced by `get_estimates(model)`. For example, applying the `get_estimates` function to a multinomial logit model returns a column called "response", which identifies the parameters that correspond to each value of the response variable:

model + response ~ term')
  }

  out <- list("lhs" = lhs,
              "rhs" = rhs,
              "group_name" = group_name)

  return(out)
}
