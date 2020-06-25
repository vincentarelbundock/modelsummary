#' Extract data from a `tables::tabular` object
#'
#' @export
#' @keywords internal
datasummary_extract <- function(tab,
                                sparse_header = TRUE) {

    out <- list()

    # NA to empty cells
    clean_na <- function(x) {
        out <- trimws(x)
        out <- ifelse(out %in% c('NA', 'NaN'), '', out)
        return(out)
    }
    
    # fill-in spanning column labels horizontally
    carry_forward <- function(x, empty = '') {
        if (length(x) > 1) {
            for (i in 2:length(x)) {
                if (x[i] == empty) {
                    x[i] <- x[i - 1]
                }
            }
        }
        x
    }
    
    # extract
    idx <- nrow(tables::colLabels(tab))
    mat <- as.matrix(tab)

    # header
    header <- mat[1:idx, , drop = FALSE]
    
    for (i in 1:nrow(header)) {
        header[i, ] <- clean_na(header[i, ])
        header[i, ] <- carry_forward(header[i, ])
    }
    
    # main --- TODO: test if main has only one row
    main <- mat[(idx + 1):nrow(mat), , drop = FALSE]
    for (i in 1:nrow(main)) {
        main[i, ] <- clean_na(main[i, ])
    }
    main <- data.frame(main)
    
    # clean columns
    colnames(main) <- clean_na(as.vector(mat[idx,]))
    
    # utility functions
    sparsify <- function(h) {
        unique_na <- function(x) length(unique(base::setdiff(x, ''))) > 1
        idx <- apply(h, 1, unique_na)  
        out <- h[idx, , drop = FALSE]
        return(out)
    }

    flatten <- function(h) {
        out <- apply(h, 2, paste, collapse = ' ')
        return(out)
    }
    
    # 1 header level means colnames are sufficient. return output immediately.
    # this needs to go before definition of header_nocolnames
    if (nrow(header) == 1) {
        out$gt <- 
        out$kableExtra <- 
        out$kableExtra_markdown <- 
        out$flextable <- 
        out$huxtable <- 
            list(main = main, span = NULL)
        return(out)
    }

    # headers matrices without colnames
    header_nocolnames <- header[1:(nrow(header) - 1), , drop = FALSE]

    # header matrices: remove rows with a single label
    header_sparse <- sparsify(header)
    header_nocolnames_sparse <- sparsify(header_nocolnames)

    # header vectors
    header_sparse_flat <- apply(header_sparse, 2, paste, collapse = ' ')
    header_nocolnames_sparse_flat <- apply(header_nocolnames_sparse, 2, paste, collapse = ' ')

    # gt only supports one span level, so we use the flat vector WITHOUT colnames
    h <- header_nocolnames_sparse_flat
    lab <- base::setdiff(unique(h), '')
    span <- list()
    for (l in lab) {
        if (trimws(l) != '') {
            pos <- which(h == l)
            span[[length(span) + 1]] <- list(label = l, position = pos)
        }
    }
    out$gt <- list(main = main, span = span)

    # kableExtra supports several span levels, so we use the matrix
    if (sparse_header) {
        h <- header_nocolnames_sparse
    } else {
        h <- header_nocolnames
    }

    clean <- function(x) ifelse(x == '', ' ', x) # needed in factory_kableExtra

    if (nrow(h) > 0) {  # are there any spans?
        span <- list()
        for (i in 1:nrow(h)) {
            z <- clean(h[i, ])
            idx <- rle(z)
            span[[i]] <- stats::setNames(idx$lengths, idx$values)
        }
    } else {
        span <- NULL
    }

    out$kableExtra <- list(main = main, span = span)

    # flextable, huxtable, and kableExtra markdown do not support spans,
    # so we use the flat vector WITH colnames
    main_flat <- main
    colnames(main_flat) <- header_sparse_flat
    out$kableExtra_markdown <- list(main = main_flat, span = NULL)
    out$flextable <- list(main = main_flat, span = NULL)
    out$huxtable <- list(main = main_flat, span = NULL)

    return(out)

}
