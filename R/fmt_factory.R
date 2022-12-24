## always exclude factors and characters, otherwise `rounding` will escape them
## which is premature since we then call coef_map

# TODO: fmt_string
# TODO: fmt_factor

fmt_statistic <- function(..., default = 3) {

    args <- modifyList(list(...), list("default" = default))
    if (!isTRUE(checkmate::check_list(args, names = "named"))) {
        msg <- "All arguments of the `fmt_statistic()` function must be named."
        insight::format_error(msg)
    }

    for (i in seq_along(args)) {
        args[[i]] <- sanitize_fmt(args[[i]])
    }

    if ("conf.int" %in% names(args)) {
        args[["conf.low"]] <- args[["conf.high"]] <- args[["conf.int"]]
    }

    out <- function(x) {

        if (!isTRUE(checkmate::check_data_frame(x))) {
            msg <- "`fmt_statistic()` only supports data frames."
            insight::format_error(msg)
        }

        for (n in colnames(x)) {
            if (is.numeric(x[[n]])) {
                if (n %in% names(args)) {
                    x[[n]] <- args[[n]](x[[n]])
                } else {
                    x[[n]] <- args[["default"]](x[[n]])
                }
                x[[n]] <- rounding_clean(x[[n]])
            }
        }
        return(x)
    }

    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_term <- function(..., default = 3) {

    args <- modifyList(list(...), list("default" = default))
    if (!isTRUE(checkmate::check_list(args, names = "named"))) {
        msg <- "All arguments of the `fmt_statistic()` function must be named."
        insight::format_error(msg)
    }

    for (i in seq_along(args)) {
        args[[i]] <- sanitize_fmt(args[[i]])
    }

    out <- function(x) {

        if (!isTRUE(checkmate::check_data_frame(x)) || !"term" %in% colnames(x)) {
            msg <- "`fmt_term()` only supports data frames with a `term` column."
            insight::format_error(msg)
        }

        atomic <- lapply(as.list(x), as.list)
        for (col in seq_along(atomic)) {
            for (row in seq_along(atomic[[col]])) {
                if (is.numeric(atomic[[col]][[row]])) {
                    if (x$term[row] %in% names(args)) {
                        atomic[[col]][[row]] <- args[[x$term[row]]](atomic[[col]][[row]])
                    } else {
                        atomic[[col]][[row]] <- args[["default"]](atomic[[col]][[row]])
                    }
                }
            }
        }

        x <- do.call("cbind", lapply(atomic, as.vector))
        x <- data.frame(x)
        for (n in colnames(x)) {
            x[[n]] <- rounding_clean(x[[n]])
        }

        return(x)
    }

    class(out) <- c("fmt_factory", class(out))
    return(out)
}



fmt_identity <- function(...) {
    out <- function(x) {
        return(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_decimal <- function(digits = 3) {
    out <- function(x) {

        # data frame
        if (isTRUE(checkmate::check_data_frame(x))) {
            for (n in colnames(x)) {
                if (n == "p.value") {
                    pdigits <- max(digits, 3)
                    # TODO: round p values
                    x[[n]] <- format(x[[n]], nsmall = digits, drop0trailing = FALSE)
                } else if (is.numeric(x[[n]])) {
                    x[[n]] <- format(x[[n]], nsmall = digits, drop0trailing = FALSE)
                }
                x[[n]] <- rounding_clean(x[[n]])
            }

        # numeric vector
        } else if (isTRUE(checkmate::check_numeric(x))) {
            # TODO: check this
             x <- format(x, nsmall = digits, drop0trailing = FALSE)
             x <- rounding_clean(x)

        # unsupported
        } else {
            msg <- "`fmt_decimal()` only supports numeric vectors and data frames."
            insight::format_error(msg)
        }

        return(x)
    }

    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_sprintf <- function(fmt) {
    out <- function(x) {
        # data frame
        if (isTRUE(checkmate::check_data_frame(x))) {
            for (n in colnames(x)) {
                if (is.numeric(x[[n]])) {
                    x[[n]] <- sprintf(fmt, x[[n]])
                    x[[n]] <- rounding_clean(x[[n]])
                }
            }

        # numeric vector
        } else if (isTRUE(checkmate::check_numeric(x))) {
            x <- sprintf(fmt, x)
            x <- rounding_clean(x)

        # unsupported
        } else {
            msg <- "`fmt_sprintf()` only supports numeric vectors and data frames."
            insight::format_error(msg)
        }

        return(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_function <- function(fun) {
    out <- function(x) {
        # data frame
        if (isTRUE(checkmate::check_data_frame(x))) {
            for (n in colnames(x)) {
                if (is.numeric(x[[n]])) {
                    x[[n]] <- fun(x[[n]])
                    x[[n]] <- rounding_clean(x[[n]])
                }
            }

        # numeric vector
        } else if (isTRUE(checkmate::check_numeric(x))) {
            x <- fun(x)
            x <- rounding_clean(x)

        # unsupported
        } else {
            msg <- "`fmt_function()` only supports numeric vectors and data frames."
            insight::format_error(msg)
        }

        return(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_significant <- function(digits = 2) {
    out <- function(x) {
        if (!isTRUE(checkmate::check_data_frame(x))) {
            msg <- "`fmt_significant` only supports data frames."
            insight::format_error(msg)
        }

        # unsupported: decimal rounding
        dec <- fmt_decimal(digits = digits)
        z <- dec(x)

        # supported: significant rounding
        cols <- c("estimate", "std.error", "conf.low", "conf.high")
        cols <- intersect(colnames(x), cols)
        tmp <- x[, cols, drop = FALSE]
        tmp <- apply(tmp, 1, FUN = function(x) format(x, digits = digits))
        tmp <- as.data.frame(t(tmp), col.names = cols)
        for (n in colnames(tmp)) {
            z[[n]] <- tmp[[n]]
        }
        return(z)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}
