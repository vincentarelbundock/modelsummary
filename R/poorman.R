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
  lapply(
    lsts,
    function(x) if (is.atomic(x) && !is_named(x)) stop("Vectors must be named.")
  )

  if (!missing(.id)) {
    lsts <- lapply(seq_along(lsts), function(i) {
      nms <- names(lsts)
      id_df <- data.frame(
        id = if (is.null(nms)) as.character(i) else nms[i],
        stringsAsFactors = FALSE
      )
      colnames(id_df) <- .id
      cbind(id_df, lsts[[i]])
    })
  }

  nms <- unique(unlist(lapply(lsts, names)))
  lsts <- lapply(
    lsts,
    function(x) {
      if (!is.data.frame(x))
        x <- data.frame(as.list(x), stringsAsFactors = FALSE)
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
is_nested <- function(lst)
  vapply(lst, function(x) inherits(x[1L], "list"), FALSE)

squash <- function(lst) {
  do.call(
    c,
    lapply(
      lst,
      function(x) if (is.list(x) && !is.data.frame(x)) squash(x) else list(x)
    )
  )
}


#' @noRd
names_are_invalid <- function(x) {
  x == "" | is.na(x)
}

#' @noRd
inner_join <- function(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  na_matches = c("na", "never")
) {
  join_worker(
    x = x,
    y = y,
    by = by,
    suffix = suffix,
    sort = FALSE,
    ...,
    keep = FALSE,
    na_matches = na_matches
  )
}

#' @noRd
left_join <- function(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  join_worker(
    x = x,
    y = y,
    by = by,
    suffix = suffix,
    all.x = TRUE,
    ...,
    keep = keep,
    na_matches = na_matches
  )
}

#' @noRd
right_join <- function(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  join_worker(
    x = x,
    y = y,
    by = by,
    suffix = suffix,
    all.y = TRUE,
    ...,
    keep = keep,
    na_matches = na_matches
  )
}

#' @noRd
full_join <- function(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  join_worker(
    x = x,
    y = y,
    by = by,
    suffix = suffix,
    all = TRUE,
    ...,
    keep = keep,
    na_matches = na_matches
  )
}

join_worker <- function(
  x,
  y,
  by = NULL,
  suffix = c(".x", ".y"),
  keep = FALSE,
  na_matches = c("na", "never"),
  ...
) {
  na_matches <- match.arg(
    arg = na_matches,
    choices = c("na", "never"),
    several.ok = FALSE
  )
  incomparables <- if (na_matches == "never") NA else NULL
  x[, ".join_id"] <- seq_len(nrow(x))
  merged <- if (is.null(by)) {
    by <- intersect(names(x), names(y))
    join_message(by)
    merge(
      x = x,
      y = y,
      by = by,
      suffixes = suffix,
      incomparables = incomparables,
      ...
    )[, union(names(x), names(y)), drop = FALSE]
  } else if (is.null(names(by))) {
    merge(
      x = x,
      y = y,
      by = by,
      suffixes = suffix,
      incomparables = incomparables,
      ...
    )
  } else {
    merge(
      x = x,
      y = y,
      by.x = names(by),
      by.y = by,
      suffixes = suffix,
      incomparables = incomparables,
      ...
    )
  }
  merged <- merged[
    order(merged[, ".join_id"]),
    colnames(merged) != ".join_id",
    drop = FALSE
  ]
  if (isTRUE(keep)) {
    keep_pos <- match(by, names(merged))
    x_by <- paste0(by, suffix[1L])
    colnames(merged)[keep_pos] <- x_by
    merged[, paste0(by, suffix[2L])] <- merged[, x_by]
  }
  rownames(merged) <- NULL
  reconstruct_attrs(merged, x)
}

join_message <- function(by) {
  if (length(by) > 1L) {
    message(
      "Joining, by = c(\"",
      paste0(by, collapse = "\", \""),
      "\")\n",
      sep = ""
    )
  } else {
    message("Joining, by = \"", by, "\"\n", sep = "")
  }
}

reconstruct_attrs <- function(data, template) {
  data <- remove_attributes(data)
  return(reconstruct_attrs_dispatch(data, template))
  UseMethod(generic = "reconstruct_attrs", object = template)
}

reconstruct_attrs_dispatch <- function(data, template) {
  UseMethod("reconstruct_attrs", template)
}

#' @export
#' @keywords internal
#' @noRd
reconstruct_attrs.data.frame <- function(data, template) {
  attrs <- attributes(template)
  attrs$names <- names(data)
  attrs$row.names <- .row_names_info(data, type = 0L)
  attributes(data) <- attrs
  data
}

remove_attributes <- function(data) {
  attrs <- attributes(data)
  foreign <- which(!names(attrs) %in% c("names", "row.names", "class"))
  if (length(foreign) > 0L) {
    for (i in foreign) {
      attr(data, names(attrs)[i]) <- NULL
    }
  }
  as.data.frame(data)
}
