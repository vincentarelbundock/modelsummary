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
        for (i in 1:span_max) {
            tmp <- sapply(span, function(x) x[i])
            lab <- setdiff(unique(tmp), " ")
            lab <- lapply(lab, function(x) {
                list("label" = x,
                     "columns" = which(x == tmp),
                     "level" = i - 1)
            })
            span_list <- c(span_list, lab)
        }
        attr(out, "column_names") <- column_names
    } else {
        out <- NULL
    }
    return(out)
}
