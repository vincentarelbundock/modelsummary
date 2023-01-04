#' sanity check
#'
#' @noRd
sanitize_shape <- function(shape) {

    if (is.null(shape)) {
        return(term + statistic ~ model)
    }

    checkmate::assert(
        checkmate::check_null(shape),
        checkmate::check_formula(shape),
        checkmate::check_choice(shape, choices = c("rbind", "rcollapse"))
    )

    # interactions with ":" are used to combine columns
    shape_cha <- deparse(shape)
    if (isTRUE(grepl("\\*|\\(", shape_cha))) {
        stop("The * and () characters are not supported in the `shape` formula.", call. = FALSE)
    }
    regex <- "\\b\\w+:\\w+\\b"
    combine <- unlist(regmatches(shape_cha, gregexpr(regex, shape_cha)))

    # remove interactions from the formula since we are going to combine them in get_estimates
    for (com in combine) {
        shape_cha <- gsub(com, gsub(":.*", "", com), shape_cha, fixed = TRUE)
    }
    shape <- stats::as.formula(shape_cha)


    lhs <- all.vars(stats::update(shape, ". ~ NULL"))
    rhs <- all.vars(stats::update(shape, "NULL ~ ."))
    lhs <- setdiff(lhs, ".")
    rhs <- setdiff(rhs, ".")
    variables <- all.vars(shape)
    shape_formula <- shape
    group_name <- setdiff(all.vars(shape_formula), c("term", "model", "statistic"))

    if (length(group_name) == 0) {
        group_name <- NULL
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
        "shape_formula" = shape_formula,
        "combine" = combine
    )

    return(out)
}


