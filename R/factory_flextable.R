#' Internal function to build table with `flextable`
#'
#' @inheritParams factory_gt
#' @return flextable object
#' @noRd
factory_flextable <- function(
  tab,
  align = NULL,
  hrule = NULL,
  hgroup = NULL,
  notes = NULL,
  title = NULL,
  output_format = "flextable",
  output_file = NULL,
  escape = TRUE,
  ...
) {
  insight::check_if_installed("flextable")

  span_list <- get_span_kableExtra(tab)

  # colnames with or without spans: before escape and for all span/no-span
  if (is.null(span_list)) {
    if (!is.null(colnames(tab))) {
      colnames(tab) <- gsub("\\|{4}", " / ", colnames(tab))
    }
  } else {
    colnames(tab) <- attr(span_list, "column_names")
  }
  colnames(tab) <- pad(colnames(tab), output_format = output_format)

  # measurements
  table_width <- ncol(tab)

  # horizontal grouping (hgroup): insert full-width group rows in the body
  has_hgroup <- !is.null(hgroup) && length(hgroup) > 0
  group_rows <- integer(0)
  if (has_hgroup) {
    # each panel's group row is inserted at that panel's first body row; a row
    # inserted at an earlier position pushes every later group row down by one.
    starts <- vapply(hgroup, function(x) x[1], numeric(1))
    group_rows <- starts + vapply(starts, function(s) sum(starts < s), integer(1))
    # insert from last to first so earlier (smaller) start indices stay valid
    for (k in order(starts, decreasing = TRUE)) {
      start <- hgroup[[k]][1]
      grp <- data.frame(matrix("", nrow = 1, ncol = ncol(tab)))
      colnames(grp) <- colnames(tab)
      grp[1, 1] <- names(hgroup)[k]
      tab <- rbind(
        tab[seq_len(start - 1), , drop = FALSE],
        grp,
        tab[start:nrow(tab), , drop = FALSE]
      )
    }
    if (!is.null(hrule)) {
      hrule <- hrule + length(hgroup)
    }
  }

  # flextable object
  out <- flextable::flextable(tab)

  # title
  if (!is.null(title)) {
    out <- flextable::set_caption(out, title)
  }

  # user-supplied notes at the bottom of table
  if (!is.null(notes)) {
    for (i in rev(seq_along(notes))) {
      out <- flextable::add_footer_row(
        out,
        values = notes[[i]],
        colwidths = table_width
      )
    }
  }

  # theme
  theme_ms <- getOption(
    "modelsummary_theme_flextable",
    default = theme_ms_flextable
  )
  out <- theme_ms(out, hrule = hrule)

  # alignment: apply before spanning headers
  if (!is.null(align)) {
    for (idx in seq_along(tab)) {
      if (idx <= length(align)) {
        alignment <- switch(align[idx],
          "c" = "center",
          "l" = "left", 
          "r" = "right",
          "center"
        )
        out <- flextable::align(out, j = idx, align = alignment, part = "all")
      }
    }
  }

  # spanning headers
  if (!is.null(span_list)) {
    for (i in seq_along(span_list)) {
      out <- flextable::add_header_row(
        out,
        colwidths = span_list[[i]],
        values = names(span_list[[i]])
      )
      # center align spanning headers
      out <- flextable::align(out, i = 1, align = "center", part = "header")
    }
  }

  # merge group rows across all columns
  if (has_hgroup) {
    for (i in group_rows) {
      out <- flextable::merge_h_range(
        out,
        i = i,
        j1 = 1,
        j2 = length(out$col_keys)
      )
    }
  }

  # output
  if (is.null(output_file)) {
    return(out)
  } else if (identical(output_format, "word")) {
    flextable::save_as_docx(out, path = output_file)
  } else if (identical(output_format, "powerpoint")) {
    flextable::save_as_pptx(out, path = output_file)
  } else if (identical(output_format, "png")) {
    flextable::save_as_image(out, path = output_file)
  } else if (identical(output_format, "html")) {
    flextable::save_as_html(out, path = output_file)
  }
}
