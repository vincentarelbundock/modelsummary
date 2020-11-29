# The functions in this file were copied as-is from the `poorman` package by
# Nathan Eastwood (copyright 2020) on 2020-11-29. They are under MIT license.



#' Efficiently bind multiple `data.frame`s by row and column
#'
#' @param ... `data.frame`s to combine.
#'
#' Each argument can either be a `data.frame`, a `list` that could be a `data.frame`, or a `list` of `data.frame`s.
#'
#' When row-binding, columns are matched by name, and any missing columns will be filled with `NA`.
#'
#' When column-binding, rows are matched by position, so all `data.frame`s must have the same number of rows. To match
#' by value, not position, see [mutate_joins].
#' @param .id `character(1)`. `data.frame` identifier.
#'
#' When `.id` is supplied, a new column of identifiers is created to link each row to its original `data.frame`. The
#' labels are taken from the named arguments to `bind_rows()`. When a `list` of `data.frame`s is supplied, the labels
#' are taken from the names of the `list`. If no names are found a numeric sequence is used instead.
#'
#' @examples
#' one <- mtcars[1:4, ]
#' two <- mtcars[9:12, ]
#'
#' # You can supply data frames as arguments:
#' bind_rows(one, two)
#'
#' # The contents of lists are spliced automatically:
#' bind_rows(list(one, two))
#' bind_rows(split(mtcars, mtcars$cyl))
#' bind_rows(list(one, two), list(two, one))
#'
#' # In addition to data frames, you can supply vectors. In the rows
#' # direction, the vectors represent rows and should have inner
#' # names:
#' bind_rows(
#'   c(a = 1, b = 2),
#'   c(a = 3, b = 4)
#' )
#'
#' # You can mix vectors and data frames:
#' bind_rows(
#'   c(a = 1, b = 2),
#'   data.frame(a = 3:4, b = 5:6),
#'   c(a = 7, b = 8)
#' )
#'
#' # When you supply a column name with the `.id` argument, a new
#' # column is created to link each row to its original data frame
#' bind_rows(list(one, two), .id = "id")
#' bind_rows(list(a = one, b = two), .id = "id")
#' bind_rows("group 1" = one, "group 2" = two, .id = "groups")
#'
#' \dontrun{
#' # Rows need to match when column-binding
#' bind_cols(data.frame(x = 1:3), data.frame(y = 1:2))
#'
#' # even with 0 columns
#' bind_cols(data.frame(x = 1:3), data.frame())
#' }
#'
#' bind_cols(one, two)
#' bind_cols(list(one, two))
#'
#' @name bind
NULL

#' @rdname bind
#' @export
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

#' @rdname bind
#' @export
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
#' @examples
#' is_nested(list(a = 1, b = 2, c = 3))
#' is_nested(list(a = 1, b = list(c = 2, d = 3)))
#' @noRd
is_nested <- function(lst) vapply(lst, function(x) inherits(x[1L], "list"), FALSE)

#' Remove all levels of a list
#' @noRd
squash <- function(lst) {
  do.call(c, lapply(lst, function(x) if (is.list(x) && !is.data.frame(x)) squash(x) else list(x)))
}

