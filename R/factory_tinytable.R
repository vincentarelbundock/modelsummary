# TODO
# span
# hrule
# hgroup
# hindent
# dot align
# tab[[i]][idx] <- gsub("<", "&lt;", tab[[i]][idx])
# tab[[i]][idx] <- gsub(">", "&gt;", tab[[i]][idx])
 

#' Internal function to build table with `tinytable`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return tinytable object
factory_tinytable <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              hgroup = NULL,
                              hindent = FALSE,
                              notes = NULL,
                              title = NULL,
                              escape = TRUE,
                              ...) {

  insight::check_if_installed("tinytable")


  # tinytable arguments
  valid <- c("x", "theme", "placement", "width", "digits", "notes", "caption")

  arguments <- list(
    caption = title,
    align = align
  )
  if (length(notes) > 1) arguments$notes <- as.list(notes)
  arguments <- c(arguments, list(...))

  # create tables with combined arguments
  arguments <- arguments[base::intersect(names(arguments), valid)]
  arguments <- c(list(tab), arguments)
  out <- do.call(tinytable::tt, arguments)

  if (isTRUE(escape)) {
    out <- tinytable::format_tt(out, escape = escape)
  }

  # align: other factories require a vector of "c", "l", "r", etc.
  # before span because those should be centered
  if (!is.null(align)) {
    l <- length(align)
    align <- paste(align, collapse = "")
    out <- tinytable::style_tt(out, j = seq_len(l), align = align)
  }

  # span: compute
  span_list <- get_span_kableExtra(tab)
  if (!is.null(span_list)) {
    column_names <- attr(span_list, "column_names")
    if (!is.null(column_names)) {
      colnames(out) <- column_names
    }
    for (i in seq_along(span_list)) {
      sp <- cumsum(span_list[[i]])
      sp <- as.list(sp)
      sp[[1]] <- 1:sp[[1]]
      sp[2:length(sp)] <- lapply(2:length(sp), function(k) (max(sp[[k - 1]]) + 1):sp[[k]])
      out <- tinytable::group_tt(out, j = sp)
      out <- tinytable::style_tt(out, i = -i, align = "c")
    }
  } else {
    colnames(out) <- gsub("\\|{4}", " / ", colnames(out))
  }


  if (!is.null(hrule)) {
    for (h in hrule) {
      out <- tinytable::style_tt(out, i = h - 1, line = "b", line_width = .05)
    }
  }

  # output
  if (is.null(settings_get("output_file"))) {
    return(out)
  } else {
    tinytable::save_tt(out, output = settings_get("output_file"), overwrite = TRUE)
  }

}
