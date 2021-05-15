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
                       output_file = NULL,
                       output_format = 'gt',
                       title = NULL,
                       ...) {

  assert_dependency("gt")

  # create gt table object
  idx_col <- ncol(tab)
  out <- gt::gt(tab)

  
  # theme
  theme_ms <- getOption("modelsummary_theme_gt",
                        default = theme_ms_gt)
  out <- theme_ms(out,
                  output_format = output_format,
                  hrule = hrule)

  # titles
  if (!is.null(title)) {
    out <- gt::tab_header(out, title = title)
  }

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
    align <- strsplit(align, '')[[1]]
    left <- grep('l', align)
    center <- grep('c', align)
    right <- grep('r', align)
    out <-
    out <- gt::cols_align(out, align = 'center', columns = center)
    out <- gt::cols_align(out, align = 'left', columns = left)
    out <- gt::cols_align(out, align = 'right', column = right)
  }

  # output
  if (is.null(output_file)) {

    if (output_format == "html") {
      out <- gt::as_raw_html(out)
    }

    if (output_format == "latex") {
      out <- gt::as_latex(out)
    }

    if (!is.null(getOption("modelsummary_orgmode")) &&
      output_format %in% c("html", "latex")) {
      out <- sprintf(
        "#+BEGIN_EXPORT %s\n%s\n#+END_EXPORT",
        output_format, out)
      return(out)
    }

    if (output_format %in% c('default', 'gt')) {
      return(out)
    }

  } else {
    gt::gtsave(out, output_file)
  }
}
