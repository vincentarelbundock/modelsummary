#' pad a character vector with spaces
#'
#' if there are duplicate or empty entries in a vector, this function adds
#' an arbitrary number of empty spaces to each entry to de-duplicate. this
#' is useful for table-making packages which require unique colnames. they
#' will usually strip white space anyway. This is suboptimal because it
#' makes it harder to select columns, so we only apply pad duplicate
#' entries.
#' @noRd
pad <- function(x, style = "unique", sep = ".") {
  checkmate::assert_character(x)
  checkmate::assert_string(sep, n.chars = 1)
  checkmate::assert_choice(style, choices = c("unique", "character"))

  if (style == "unique") {
    out <- trimws(x)
    out[out == ""] <- " "
    tab <- table(out)
    for (i in seq_along(tab)) {
      if (tab[i] > 1) {
        idx <- which(out == names(tab)[i])
        tmp <- sapply(0:(length(idx) - 1), function(k) {
          paste0(names(tab)[i], strrep(" ", k))
        })
        out[idx] <- tmp
      }
    }

  } else if (style == "character") {
    # split at character or last digit
    nosep <- !grepl(sep, x, fixed = TRUE)
    x <- ifelse(
      nosep,
      gsub("(.*)([0-9])([^0-9]*)", paste0("\\1\\2", sep, "\\3"), x),
      x)

    # split but make sure there are two elements
    x_split <- strsplit(x, sep, fixed = TRUE)
    for (i in seq_along(x_split)) {
      if (length(x_split[[i]]) == 1) {
        x_split[[i]] <- c(x_split[[i]], "")
      }
    }

    left_long <- max(sapply(x_split, function(i) ifelse(length(i) == 2, nchar(i[[1]], type = "width"), 0)))
    right_long <- max(sapply(x_split, function(i) ifelse(length(i) == 2, nchar(i[[2]], type = "width"), 0)))
    left_long <- max(ceiling((nchar(x, type = "width") - 1) / 2), left_long)
    right_long <- max(ceiling((nchar(x, type = "width") - 1) / 2), right_long)

    for (i in seq_along(x_split)) {

      if (settings_equal("output_format", "markdown")) {
        padchar <- " "
      } else {
        padchar <- "\U02007"
      }
      # rounded numeric (no decimal): left-centered
      if (length(x_split[[i]]) == 1) {
        is_rounded <- !is.na(suppressWarnings(as.numeric(x_split[[i]])))
        if (is_rounded) {
          left <- x_split[[i]]
          right <- ""
          left <- paste0(strrep(padchar, left_long - nchar(left, type = "width")), left)
          right <- paste0(strrep(padchar, right_long - nchar(right, type = "width") + 1))
          x_split[[i]] <- paste0(left, right)
        } else {
          x_split[[i]] <- x[[i]]
        }

        # decimal numeric
      } else if (length(x_split[[i]]) == 2) {
        left <- x_split[[i]][[1]]
        right <- x_split[[i]][[2]]
        left <- paste0(strrep(padchar, left_long - nchar(left, type = "width")), left)
        right <- paste0(right, strrep(padchar, right_long - nchar(right, type = "width")))
        x_split[[i]] <- paste0(left, sep, right)


        # string or unknown
      } else {
        x_split[[i]] <- x[[i]]
      }
    }
    out <- unlist(x_split)
    out <- ifelse(nosep, gsub(sep, "", out, fixed = TRUE), out)
  }

  return(out)
}
