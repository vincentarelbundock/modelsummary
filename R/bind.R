# The functions in this file were copied as-is from the `poorman` package by
# Nathan Eastwood (copyright 2020) on 2020-11-29. They are under MIT license.


#' bind columns
#' @noRd
bind_cols <- function(...) {
  lsts <- list(...)
  lsts <- squash(lsts)
  lsts <- Filter(Negate(is.null), lsts)
  if (length(lsts) == 0L) return(data.frame())
  lapply(lsts, function(x) is_df_or_vector(x))
  lsts <- do.call(cbind, lsts)
  if (!is.data.frame(lsts)) lsts <- as.data.frame(lsts)
  lsts
}

#' bind rows
#' @noRd
bind_rows <- function(..., .id = NULL) {
  lsts <- list(...)
  lsts <- flatten(lsts)
  lsts <- Filter(Negate(is.null), lsts)
  lapply(lsts, function(x) is_df_or_vector(x))
  lapply(lsts, function(x) if (is.atomic(x) && !is_named(x)) stop("Vectors must be named."))

  if (!missing(.id)) {
    lsts <- lapply(seq_along(lsts), function(i) {
      nms <- names(lsts)
      id_df <- data.frame(id = if (is.null(nms)) as.character(i) else nms[i], stringsAsFactors = FALSE)
      colnames(id_df) <- .id
      cbind(id_df, lsts[[i]])
    })
  }

  nms <- unique(unlist(lapply(lsts, names)))
  lsts <- lapply(
    lsts,
    function(x) {
      if (!is.data.frame(x)) x <- data.frame(as.list(x), stringsAsFactors = FALSE)
      for (i in nms[!nms %in% names(x)]) x[[i]] <- NA
      x
    }
  )
  names(lsts) <- NULL
  do.call(rbind, lsts)
}

#' Move entries within a list up one level
#' @noRd
flatten <- function(lst) {
  nested <- is_nested(lst)
  res <- c(lst[!nested], unlist(lst[nested], recursive = FALSE))
  if (sum(nested)) Recall(res) else return(res)
}


#' Check whether the input is an atomic vector or a data.frame
#' @noRd
is_df_or_vector <- function(x) {
  res <- is.data.frame(x) || is.atomic(x)
  if (isFALSE(res)) stop("You must pass vector(s) and/or data.frame(s).")
  TRUE
}

#' poorman check
#' @noRd
is_named <- function(x) {
  nms <- names(x)
  if (is.null(nms)) return(FALSE)
  if (any(names_are_invalid(nms))) return(FALSE)
  TRUE
}

#' Check whether the input is an atomic vector or a data.frame
#' @noRd
is_df_or_vector <- function(x) {
  res <- is.data.frame(x) || is.atomic(x)
  if (isFALSE(res)) stop("You must pass vector(s) and/or data.frame(s).")
  TRUE
}

#' Check whether any elements of a list are nested#' @param lst A `list()`
#' @noRd
is_nested <- function(lst) vapply(lst, function(x) inherits(x[1L], "list"), FALSE)

#' Remove all levels of a list
#' @noRd
squash <- function(lst) {
  do.call(c, lapply(lst, function(x) if (is.list(x) && !is.data.frame(x)) squash(x) else list(x)))
}


#' poorman check
#' @noRd
names_are_invalid <- function(x) {
  x == "" | is.na(x)
}
