# The functions in this file were copied as-is from the `poorman` package by
# Nathan Eastwood on 2020-11-29. They are under MIT license.

# License: MIT + file LICENSE

# Copyright (c) 2020, Nathan Eastwood

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


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
