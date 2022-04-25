get_span_kableExtra <- function(tab) {
    span_list <- attr(tab, "span_kableExtra")
    if (!is.null(span_list)) return(span_list)

    flag <- any(grepl("\\|\\|\\|\\|", colnames(tab)))
    if (isTRUE(flag)) {
        span_list <- list()
        span <- strsplit(colnames(tab), "\\|\\|\\|\\|")
        span <- lapply(span, rev)
        span_max <- max(sapply(span, length))
        span <- lapply(span, function(x) c(x, rep(" ", span_max - length(x))))
        column_names <- sapply(span, function(x) x[1])
        for (i in 2:span_max) {
            tmp <- sapply(span, function(x) x[i])
            tmp <- rle(tmp)
            span_list[[i - 1]] <- stats::setNames(tmp$lengths, tmp$values)
        }
        out <- span_list
        attr(out, "column_names") <- column_names
    } else {
        out <- NULL
    }
    return(out)
}


get_span_gt <- function(tab) {
    span_list <- attr(tab, "span_gt")
    if (!is.null(span_list)) return(span_list)

    flag <- any(grepl("\\|{4}", colnames(tab)))
    if (isTRUE(flag)) {
        span_list <- list()
        span <- strsplit(colnames(tab), "\\|\\|\\|\\|")
        span <- lapply(span, rev)
        span_max <- max(sapply(span, length))
        span <- lapply(span, function(x) c(x, rep(" ", span_max - length(x))))
        column_names <- pad(sapply(span, function(x) x[1]))
        for (i in 2:span_max) {
            tmp <- sapply(span, function(x) x[i])
            lab <- setdiff(unique(tmp), " ")
            # tab_spanner(columns) must be a consecutive series
            consecutive <- function(v) split(v, cumsum(c(1, diff(v) != 1)))
            for (l in lab) {
              idx <- which(l == tmp)
              idx <- consecutive(idx)
              l_pad <- pad(rep(l, length(idx))) # no dups allowed by gt
              spa <- lapply(seq_along(idx), function(k)
                            list(level = i - 1,
                                 # HACK: pad with row-specific empty space to avoid gt check
                                 label = paste0(l_pad[k], strrep(" ", 2 * i)),
                                 columns = idx[[k]]))
              span_list <- c(span_list, spa)
            }
        }
        out <- span_list
        attr(out, "column_names") <- column_names
    } else {
        out <- NULL
    }
    return(out)
}
