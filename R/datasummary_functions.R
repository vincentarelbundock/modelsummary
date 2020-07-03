
#' datasummary statistic shortcut
#' @export
#' @keywords internal
Mean <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- mean(x, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Median <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::median(x, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Min <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- min(x, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Max <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- max(x, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
SD <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::sd(x, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Var <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::var(x, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Ncol <- function(x, ...) 
    paste0('(N = ', length(x), ')')

#' datasummary statistic shortcut
#' @export
#' @keywords internal
NPercent <- function(x, y) {
    pct <- round(100 * length(x) / length(y))
    n <- length(x)
    if (n == 0) {
        out <- '.'
    } else {
        out <- paste0(n, ' (', pct, '%)')
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
NUnique <- function(x, ...) length(unique(x))


#' datasummary statistic shortcut
#' @export
#' @keywords internal
P0 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::quantile(x, prob = 0, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P25 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::quantile(x, prob = 0.25, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P50 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::quantile(x, prob = 0.50, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P75 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::quantile(x, prob = 0.75, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P100 <- function(x, fmt = NULL, na.rm = TRUE, ...) {
    out <- stats::quantile(x, prob = 1, na.rm = na.rm)
    if (!is.null(fmt)) {
        out <- sprintf(fmt, out)
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
PercentMissing <- function(x, fmt = NULL) { 
    out <- mean(is.na(x)) * 100
    if ((!is.null(fmt))) {
        out <- sprintf(fmt, )
    }
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Histogram <- function(x, bins = 10) {
    #ticks <- c(" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█")
    ticks <- c(" ", "\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588")
    ticks_values <- seq(0, 1, length.out = length(ticks))
    barheight <- cut(x, breaks = bins, labels = FALSE) 
    barheight <- table(barheight)
    barheight <- barheight / max(barheight) * 7 + 1
    barheight <- round(barheight)
    bars <- ticks[barheight]
    out <- paste(bars, collapse = '')
    return(out)
}

#' datasummary statistic shortcut
#' @export
#' @keywords internal
DinM <- function(x, data, statistic = 'estimate', fmt = "%.2f") {
    # is estimatr installed?
    if (!requireNamespace('estimatr', quietly = TRUE)) {
        stop("Please install the `estimatr` package.")
    }
    
    # if clusters, weights, or blocks are absent, we set them to NULL
    if (!'clusters' %in% colnames(data)) clusters <- NULL
    if (!'weights' %in% colnames(data)) weights <- NULL
    if (!'blocks' %in% colnames(data)) blocks <- NULL
    
    # datasummary supplies a numeric vector. we use it as outcome
    data$outcome_variable <- x
    
    # compute statistics
    out <- estimatr::difference_in_means(outcome_variable ~ condition_variable, 
                                         data = data, 
                                         blocks = blocks,
                                         clusters = clusters, 
                                         weights = weights)
    
    # extract result using estimatr's `tidy` function
    out <- estimatr::tidy(out)[[statistic]]
    return(out)
}
