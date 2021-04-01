#' sanity check
#'
#' @noRd
sanitize_group <- function(group) {

    if (is.null(group)) return(group)

    checkmate::assert_formula(group, null.ok = TRUE)

    rhs <- all.vars(stats::update(group, "0 ~ ."))
    lhs <- all.vars(stats::update(group, ". ~ 0"))
    group_name <- setdiff(c(lhs, rhs), c("term", "model"))

    if (!all(c("term", "model") %in% c(lhs, rhs)) ||
        length(unique(c(lhs, rhs))) != 3) {
        stop('The `group` argument must be a two-sided formula with three components: "term", "model", and a group identifier. The group identifier must be the name of a column in the data.frame produced by `get_estimates(model)`. The "term" component must be on the left-hand side of the formula. ')
    }

    out <- list("lhs" = lhs,
                "rhs" = rhs,
                "group_name" = group_name)
    return(out)
}
