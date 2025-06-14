#' Internal function to build table with `flextable`
#'
#' @inheritParams factory_gt
#' @return flextable object
#' @noRd
factory_flextable <- function(
  tab,
  align = NULL,
  hrule = NULL,
  notes = NULL,
  title = NULL,
  output_format = "flextable",
  output_file = NULL,
  ...
) {
  insight::check_if_installed("flextable")

  span <- get_span_kableExtra(tab)
  colnames(tab) <- gsub(".*\\|\\|\\|\\|", "", colnames(tab))
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

  # spanning headers
  for (i in seq_along(span)) {
    out <- flextable::add_header_row(
      out,
      colwidths = span[[i]],
      values = names(span[[i]])
    )
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
