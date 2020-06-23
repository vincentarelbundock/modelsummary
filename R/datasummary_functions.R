
#' datasummary statistic shortcut
#' @export
#' @keywords internal
Mean <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, mean(x, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
MeanSD <- function(x, fmt = '%.1f', na.rm = TRUE) {
    m <- mean(x, na.rm = na.rm)
    s <- stats::sd(x, na.rm = na.rm)
    m <- sprintf(fmt, m)
    s <- sprintf(fmt, s)
    out <- paste0(m, ' (', s, ')')
    return(out)
}

#' datasummary statistic shortcut
#' @importFrom stats median
#' @export
#' @keywords internal
Median <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, median(x, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Min <- function(x, fmt = '%.1f', na.rm = TRUE) sprintf(fmt, min(x, na.rm = TRUE))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Max <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, max(x, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
SD <- function(x, fmt = '%.1f', na.rm = TRUE, ...)
    sprintf(fmt, stats::sd(x, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Var <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, stats::var(x, na.rm = na.rm))

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
P0 <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, stats::quantile(x, prob = 0, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P25 <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, stats::quantile(x, prob = .25, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P50 <- function(x, fmt = '%.1f', na.rm = TRUE) 
    sprintf(fmt, stats::quantile(x, prob = .5, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P75 <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, stats::quantile(x, prob = .75, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
P100 <- function(x, fmt = '%.1f', na.rm = TRUE, ...) 
    sprintf(fmt, stats::quantile(x, prob = 1, na.rm = na.rm))

#' datasummary statistic shortcut
#' @export
#' @keywords internal
PercentMissing <- function(x, fmt = '%.1f', ...) 
    sprintf(fmt, mean(is.na(x)) * 100)

#' datasummary statistic shortcut
#' @export
#' @keywords internal
Histogram <- function(x, bins = 10) {
    #ticks <- c(" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█")
    ticks <- c(" ", "\\u2581", "\\u2582", "\\u2583", "\\u2584", "\\u2585",
"\\u2586", "\\u2587", "\\u2588")
    ticks_values <- seq(0, 1, length.out = length(ticks))
    barheight <- cut(x, breaks = bins, labels = FALSE) 
    barheight <- table(barheight)
    barheight <- barheight / max(barheight) * 7 + 1
    barheight <- round(barheight)
    bars <- ticks[barheight]
    out <- paste(bars, collapse = '')
    return(out)
}

