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

  # horizontal grouping (hgroup)
  if (!is.null(hgroup) && length(hgroup) > 0) {
    # hgroup creates grouped rows in the body
    for (group_name in names(hgroup)) {
      group_rows <- hgroup[[group_name]]
      if (length(group_rows) > 0) {
        # add a row above the first row of each group
        first_row <- min(group_rows)
        out <- flextable::add_header_lines(out, values = group_name)
        out <- flextable::align(out, i = 1, align = "left", part = "header")
      }
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
