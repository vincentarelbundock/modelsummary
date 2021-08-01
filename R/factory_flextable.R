#' Internal function to build table with `flextable`
#'
#' @inheritParams factory_gt
#' @return flextable object
#' @noRd
factory_flextable <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              notes = NULL,
                              title = NULL,
                              ...) {

  assert_dependency("flextable")

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
    for (i in seq_along(notes)) {
      out <- flextable::add_footer_row(out,
        values = notes[[i]],
        colwidths = table_width)
    }
  }

  # theme
  theme_ms <- getOption("modelsummary_theme_flextable",
                        default = theme_ms_flextable)
  out <- theme_ms(out, hrule = hrule)

  # output
  if (is.null(settings_get("output_file"))) {
    return(out)
  } else if (settings_equal("output_format", "word")) {
    flextable::save_as_docx(out, path = settings_get("output_file"))
  } else if (settings_equal("output_format", "powerpoint")) {
    flextable::save_as_pptx(out, path = settings_get("output_file"))
  } else if (settings_equal("output_format", "png")) {
    flextable::save_as_image(out, path = settings_get("output_file"))
  } else if (settings_equal("output_format", "html")) {
    flextable::save_as_html(out, path = settings_get("output_file"))
  }

}
