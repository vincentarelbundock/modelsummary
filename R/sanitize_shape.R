#' sanity check
#'
#' @noRd
sanitize_shape <- function(shape, ...) {

    dots <- list(...)
    # backward compatibility
    if (isTRUE("group" %in% names(dots))) {
        # shape is not default, means user changed it
        if (!isTRUE(all.equal(shape, term + statistic ~ model))) {
            msg <- "The `group` argument was deprecated and renamed `shape`. The two arguments should be used simultaneously."
            stop(msg, call. = FALSE)
        # shape is default, so we use `group` for backward compatibility
        } else {
            shape <- dots[["group"]]
        }
    }

    checkmate::assert_class(shape, "formula")

    lhs <- all.vars(stats::update(shape, ". ~ NULL"))
    rhs <- all.vars(stats::update(shape, "NULL ~ ."))
    lhs <- setdiff(lhs, ".")
    rhs <- setdiff(rhs, ".")
    variables <- all.vars(shape)
    shape_formula <- shape
    group_name <- setdiff(all.vars(shape_formula), c("term", "model", "statistic"))

    if (length(group_name) == 0) {
        group_name <- NULL
    } else if (length(group_name) == 1) {
        lhs[lhs == group_name] <- "group"
        rhs[rhs == group_name] <- "group"
    } else {
        msg <- 'The `shape` formula can only include one group name. The other terms must be: "term", "model", or "statistic".'
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

    shape_formula <- stats::as.formula(
        paste(paste(lhs, collapse = "+"),
              "~",
              paste(rhs, collapse = "+")))

    out <- list(
        "lhs" = lhs,
        "rhs" = rhs,
        "group_name" = group_name,
        "shape_formula" = shape_formula
    )

    return(out)
}
