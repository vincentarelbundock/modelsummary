#' Rounding with a user-supplied function in the `fmt` argument
#' 
#' @noRd
#' @param fun A function which accepts a numeric vector and returns a numeric vector of the same length.
fmt_function <- function(fun) {
    out <- function(x, ...) {
        if (isTRUE(checkmate::check_data_frame(x))) {
            for (n in colnames(x)) {
                if (is.numeric(x[[n]])) {
                    x[[n]] <- fun(x[[n]])
                    x[[n]] <- fmt_nainf(x[[n]])
                    x[[n]] <- fmt_mathmode(x[[n]])
                }
            }
        } else if (isTRUE(checkmate::check_numeric(x))) {
            x <- fun(x)
            x <- fmt_nainf(x)
            x <- fmt_mathmode(x)
        }
        return(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


#' Rounding with the `sprintf()` function in the `fmt` argument
#' 
#' @param fmt A string to control `sprintf()`, such as `"%.3f"` to keep 3 decimal digits. See `?sprintf`
#' @export
fmt_sprintf <- function(fmt) {
    fun <- function(k) sprintf(fmt, k)
    out <- function(x, ...) {
        fmt_function(fun)(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_identity <- function(...) {
    out <- function(x) return(x)
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


#' Rounding with decimal digits in the `fmt` argument
#' 
#' @param digits Number of decimal digits to keep, including trailing zeros.
#' @param pdigits Number of decimal digits to keep for p values. If `NULL`, the value of `digits` is used.
#' @param ... Additional arguments are passed to the `format()` function (e.g., `big.marks`, `scientific`). See `?format`
#' @export
fmt_decimal <- function(digits = 3, pdigits = NULL, ...) {

    if (is.null(pdigits)) {
        pdigits <- digits
    }

    out <- function(x, pval = FALSE, ...) {
        fun <- function(z, ...) {
            format(round(z, digits), nsmall = digits, drop0trailing = FALSE, trim = TRUE, ...)
        }
        pfun <- function(z, ...) {
            threshold <- 10^-pdigits
            ifelse(z < threshold,
                   paste0("<", format(round(threshold, pdigits), trim = TRUE)),
                   format(round(z, pdigits), nsmall = pdigits, trim = TRUE), ...)
        }

        if (isTRUE(checkmate::check_data_frame(x))) {
            for (n in colnames(x)) {
                if (is.numeric(x[[n]])) {
                    if (n == "p.value") {
                        x[[n]] <- pfun(x[[n]], ...)
                    } else {
                        x[[n]] <- fun(x[[n]], ...)
                    }
                    x[[n]] <- fmt_nainf(x[[n]])
                    x[[n]] <- fmt_mathmode(x[[n]])
                } else {
                    x[[n]] <- as.character(x[[n]])
                    x[[n]] <- fmt_nainf(x[[n]])
                }
            }

        } else if (isTRUE(checkmate::check_numeric(x))) {
            if (isTRUE(pval)) {
                x <- pfun(x, ...)
            } else {
                x <- fun(x, ...)
            }
            x <- fmt_nainf(x)
            x <- fmt_mathmode(x)
        }

        return(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}


#' Rounding with significant digits in the `fmt` argument
#' 
#' The number of decimal digits to keep after the decimal is assessed 
#' @param digits Number of significant digits to keep.
#' @param ... Additional arguments are passed to the `format()` function (e.g., `big.marks`, `scientific`). See `?format`
#' @export
fmt_significant <- function(digits = 3, ...) {
    fun <- function(x) format(x, digits = digits, trim = TRUE, ...)
    out <- function(x) {
        if (isTRUE(checkmate::check_data_frame(x))) {
            # unsupported columns/statistics: decimal rounding
            z <- fmt_decimal(digits = digits)(x)
            # supported columns/statistics: significant rounding
            cols <- c("estimate", "std.error", "conf.low", "conf.high")
            cols <- intersect(colnames(x), cols)
            tmp <- x[, cols, drop = FALSE]
            tmp <- apply(tmp, 1, FUN = fun)
            tmp <- as.data.frame(t(tmp), col.names = cols)
            for (n in colnames(tmp)) {
                z[[n]] <- tmp[[n]]
                z[[n]] <- fmt_nainf(z[[n]])
                z[[n]] <- fmt_mathmode(z[[n]])
            }
            x <- z
        } else if (isTRUE(checkmate::check_numeric(x))) {
            x <- fun(x)
            x <- fmt_nainf(x)
            x <- fmt_mathmode(x)
        }
        return(x)
    }
    class(out) <- c("fmt_factory", class(out))
    return(out)
}



#' Rounding with decimal digits on a per-statistic basis in the `fmt` argument for `modelsummary()`
#' 
#' @param ... Statistic names and `fmt` value
#' @param default Number of decimal digits to keep for unspecified terms
#' @export
fmt_statistic <- function(..., default = 3) {

    args <- utils::modifyList(list(...), list("default" = default))
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
                x[[n]] <- fmt_nainf(x[[n]])
                x[[n]] <- fmt_mathmode(x[[n]])
            }
        }
        return(x)
    }

    class(out) <- c("fmt_factory", class(out))
    return(out)
}


#' Rounding with decimal digits on a per-term basis in the `fmt` argument for `modelsummary()`
#' 
#' @param ... Term names and `fmt` value
#' @param default Number of decimal digits to keep for unspecified terms
#' @export
fmt_term <- function(..., default = 3) {

    args <- utils::modifyList(list(...), list("default" = default))
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
                    atomic[[col]][[row]] <- fmt_nainf(atomic[[col]][[row]])
                    atomic[[col]][[row]] <- fmt_mathmode(atomic[[col]][[row]])
                }
            }
        }

        x <- do.call("cbind", lapply(atomic, as.vector))
        x <- data.frame(x)

        return(x)
    }

    class(out) <- c("fmt_factory", class(out))
    return(out)
}


fmt_mathmode <- function(x) {
    out <- x

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

    # we usually run fmt_mathmode before this
    out <- gsub("\\\\num\\{\\}", "", out)

    return(out)
}


fmt_nainf <- function(x) {
    if (is.factor(x) || is.logical(x) || is.character(x)) {
        x <- as.character(x)
    }

    # Remove weird numbers before wrapping in siunitx
    out <- gsub("^NA$|^NaN$|^-Inf$|^Inf$", "", x)

    # empty siunitx
    out <- gsub("\\\\num\\{\\}", "", out)

    return(out)
}