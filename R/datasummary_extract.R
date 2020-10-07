#' Extract data from a `tables::tabular` object
#'
#' @keywords internal
datasummary_extract <- function(tab,
                                fmt = NULL,
                                sparse_header = TRUE) {


  # arbitrary length of no formatting requested
  if (is.null(fmt)) fmt <- '%.50f'

  # formatting specified in the tabular formula
  idx <- unlist(attr(tab, 'format'))
  for (i in seq_along(tab)) {
    # use default where not specified in the tabular formula
    if (is.na(idx[i])) {
      if (is.numeric(tab[[i]])) {
        tab[[i]] <- rounding(tab[[i]], fmt)
      }
    }
  }

  # output object
  out <- list()

  # NA to empty cells
  clean_na <- function(x) {
    out <- trimws(x)
    out[is.na(out)] <- ''
    out <- ifelse(out %in% c('NA', 'NaN'), '', out)
    return(out)
  }

  # fill-in spanning column labels horizontally
  carry_forward <- function(x, empty = '') {
    out <- trimws(x)
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        if (is.na(x[i])) {
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
  header <- as.matrix(tables::colLabels(tab))
  for (i in 1:nrow(header)) {
    header[i, ] <- carry_forward(header[i, , drop = TRUE])
    header[i, ] <- clean_na(header[i, , drop = TRUE])
  }

  stub_width <- ncol(mat) - ncol(header)

  pad_header <- matrix('',
    nrow = nrow(header),
    ncol = stub_width)
  header <- cbind(pad_header, header)

  # main --- TODO: test if main has only one row
  main <- mat[(idx + 1):nrow(mat), , drop = FALSE]
  for (i in 1:nrow(main)) {
    main[i, ] <- clean_na(main[i, ])
  }
  main <- data.frame(main)

  # clean columns
  colnames(main) <- clean_na(as.vector(mat[idx, ]))

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

  # stub_width attribute before return
  attr(main, 'stub_width') <- stub_width

  # 1 header level means colnames are sufficient. return output immediately.
  # this needs to go before definition of header_nocolnames
  if (nrow(header) == 1) {
    return(main)
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
  attr(main, 'span_gt') <- span

  # kableExtra supports several span levels, so we use the matrix
  if (sparse_header) {
    h <- header_nocolnames_sparse
  } else {
    h <- header_nocolnames
  }

  clean <- function(x) ifelse(x == '', ' ', x) # needed in factory_kableExtra

  if (nrow(h) > 0) { # are there any spans?
    span <- list()
    for (i in 1:nrow(h)) {
      z <- clean(h[i, ])
      idx <- rle(z)
      span[[i]] <- stats::setNames(idx$lengths, idx$values)
    }
  } else {
    span <- NULL
  }
  attr(main, 'span_kableExtra') <- span

  # attributes
  attr(main, 'header_sparse_flat') <- header_sparse_flat

  # return
  return(main)

}
