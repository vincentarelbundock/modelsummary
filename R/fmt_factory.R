# TODO: p values
# TODO: fmt_significant should still work for DF where there is no "term" column

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
                x[[n]] <- fmt_clean(x[[n]])
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
            x[[n]] <- fmt_clean(x[[n]])
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


fmt_decimal <- function(digits = 3, pdigits = NULL) {
    if (is.null(pdigits)) {
        pdigits <- digits
    }
    out <- function(x) {
        # data frame
        if (isTRUE(checkmate::check_data_frame(x))) {
            for (n in colnames(x)) {
                if (is.numeric(x[[n]])) {
                    if (n == "p.value") {
                        th <- 10^-pdigits
                        x[[n]] <- ifelse(
                            x[[n]] < th,
                            paste0("<", format(round(th, pdigits), trim = TRUE)),
                            format(round(x[[n]], pdigits), nsmall = pdigits, trim = TRUE))
                    } else {
                        x[[n]] <- format(round(x[[n]], digits), nsmall = digits, drop0trailing = FALSE, trim = TRUE)
                    }
                } else {
                    x[[n]] <- as.character(x[[n]])
                    ## e scape
                    if (settings_equal("escape", TRUE)) {
                        x[[n]] <- escape_string(x[[n]])
                    }
                }
                x[[n]] <- fmt_clean(x[[n]])
            }

        # numeric vector
        } else if (isTRUE(checkmate::check_numeric(x))) {
            # TODO: check this
            x <- format(round(x, digits), nsmall = digits, drop0trailing = FALSE, trim = TRUE)
            x <- fmt_clean(x)
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
                    x[[n]] <- fmt_clean(x[[n]])
                }
            }

        # numeric vector
        } else if (isTRUE(checkmate::check_numeric(x))) {
            x <- sprintf(fmt, x)
            x <- fmt_clean(x)
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
                    x[[n]] <- fmt_clean(x[[n]])
                }
            }

        # numeric vector
        } else if (isTRUE(checkmate::check_numeric(x))) {
            x <- fun(x)
            x <- fmt_clean(x)

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
        tmp <- apply(tmp, 1, FUN = function(x) format(x, digits = digits, trim = TRUE))
        tmp <- as.data.frame(t(tmp), col.names = cols)
        for (n in colnames(tmp)) {
            z[[n]] <- tmp[[n]]
        }
        return(z)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_clean <- function(x) {
    if (is.factor(x) || is.logical(x) || is.character(x)) {
        x <- as.character(x)
    }

    # Remove weird numbers before wrapping in siunitx
    out <- gsub("^NA$|^NaN$|^-Inf$|^Inf$", "", x)

    ## LaTeX siunitx \num{}
    if (settings_equal("output_format", c("latex", "latex_tabular"))) {
        if (!isTRUE(settings_get("siunitx_scolumns"))) {
            if (settings_equal("format_numeric_latex", "siunitx") && !settings_equal("dcolumn_stars_mbox", TRUE)) {
                out <- sprintf("\\num{%s}", out)
            } else if (settings_equal("format_numeric_latex", c("dollars", "mathmode"))) {
                out <- sprintf("$%s$", out)
            }
        }
    }

    ## HTML: convert hyphen-minus to minus
    if (settings_equal("output_format", c("html", "kableExtra"))) {
        # in hebrew or chinese locales, the html minus signs does not appear and it underlines the whole number.
        # https://github.com/vincentarelbundock/modelsummary/issues/552
        if (settings_equal("modelsummary_format_numeric_html", "minus") && settings_equal("known_locale", TRUE)) {
            out <- gsub("\\-", "\u2212", out)
        } else if (settings_equal("modelsummary_format_numeric_html", c("mathjax", "dollars"))) {
            out <- sprintf("$%s$", out)
        }
    }

    # empty siunitx
    out <- gsub("\\\\num\\{\\}", "", out)

    return(out)
}