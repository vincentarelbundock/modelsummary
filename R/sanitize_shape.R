#' sanity check
#'
#' @noRd
sanitize_shape <- function(shape) {

    if (is.null(shape)) {
        return(term + statistic ~ model)
    }

    checkmate::assert_class(shape, "formula", null.ok = TRUE)

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
