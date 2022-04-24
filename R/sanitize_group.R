#' sanity check
#'
#' @noRd
sanitize_group <- function(group) {
    checkmate::assert_class(group, "formula")

    lhs <- all.vars(update(group, ". ~ NULL"))
    rhs <- all.vars(update(group, "NULL ~ ."))
    lhs <- setdiff(lhs, ".")
    rhs <- setdiff(rhs, ".")
    variables <- all.vars(group)
    group_formula <- group
    group_name <- setdiff(all.vars(group_formula), c("term", "model", "statistic"))


    if (length(group_name) == 0) {
        group_name <- NULL
    } else if (length(group_name) == 1) {
        lhs[lhs == group_name] <- "group"
        rhs[rhs == group_name] <- "group"
    } else {
        msg <- 'The `group` formula can only include one group name. The other terms must be: "term", "model", or "statistic".'
        stop(msg, call. = FALSE)
    }

    # partial formulas
    if (!"term" %in% variables && !"statistic" %in% variables) {
        lhs <- c("term", lhs, "statistic")
    } else if (!"statistic" %in% variables) {
        lhs <- c(lhs, "statistic")
    } else if (!"term" %in% variables) {
        lhs <- c("term", lhs)
    }
    if (!"model" %in% variables) {
        rhs <- c("model", rhs)
    }

    group_formula <- as.formula(paste(paste(lhs, collapse = "+"),
                                      "~",
                                      paste(rhs, collapse = "+")))

    out <- list(
        "lhs" = lhs,
        "rhs" = rhs,
        "group_name" = group_name,
        "group_formula" = group_formula
    )

    return(out)
}
