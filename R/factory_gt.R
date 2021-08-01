#' Internal function to build table with `gt`
#'
#' @inheritParams factory
#' @param output_file file path (character or NULL)
#' @param output_format character
#' @noRd
#' @return tbl_gt object
factory_gt <- function(tab,
                       align = NULL,
                       hrule = NULL,
                       notes = NULL,
                       title = NULL,
                       ...) {

  assert_dependency("gt")

  # create gt table object
  idx_col <- ncol(tab)
  out <- gt::gt(tab, caption = title)


  # theme
  theme_ms <- getOption("modelsummary_theme_gt",
                        default = theme_ms_gt)
  out <- theme_ms(out,
                  output_format = settings_get("output_format"),
                  hrule = hrule)

  # user-supplied notes at the bottom of table
  if (!is.null(notes)) {
    for (n in notes) {
      out <- gt::tab_source_note(out, source_note = n)
    }
  }

  # column span labels
  span <- attr(tab, 'span_gt')

  if (!is.null(span)) {
    for (s in span) {
      out <- gt::tab_spanner(out, label = s$label, columns = s$position)
    }
  }

  # column alignment
  if (!is.null(align)) {
    left <- grep('l', align)
    center <- grep('c', align)
    right <- grep('r', align)
    out <-
    out <- gt::cols_align(out, align = 'center', columns = center)
    out <- gt::cols_align(out, align = 'left', columns = left)
    out <- gt::cols_align(out, align = 'right', column = right)
  }

  # output
  if (is.null(settings_get("output_file"))) {

    if (settings_equal("output_format", "html")) {
      out <- gt::as_raw_html(out)
    }

    if (settings_equal("output_format", "latex")) {
      out <- gt::as_latex(out)
    }

    if (!is.null(getOption("modelsummary_orgmode")) &&
        settings_equal("output_format", c("html", "latex"))) {
      out <- sprintf("#+BEGIN_EXPORT %s\n%s\n#+END_EXPORT", settings_get("output_format"), out)
      return(out)
    }

    if (settings_equal("output_format", c("default", "gt"))) {
      return(out)
    }

  } else {
    gt::gtsave(out, settings_get("output_file"))
  }
}
