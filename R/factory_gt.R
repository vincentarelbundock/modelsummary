#' Internal function to build table with `gt`
#'
#' @inheritParams factory
#' @param output_file file path (character or NULL)
#' @param output_format character
#' @keywords internal
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

  # horizontal rule to separate coef/gof
  if (!is.null(hrule)) { # check if there are >0 GOF
    for (pos in hrule) {
      out <- gt::tab_style(
        out, 
        style = gt::cell_borders(sides = 'bottom', color = '#000000'),
        locations = gt::cells_body(columns = 1:idx_col, rows = (pos - 1))
      )
    }
  }

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
    out <- gt::cols_align(out, align='center', columns=center)
    out <- gt::cols_align(out, align='left', columns=left) 
    out <- gt::cols_align(out, align='right', column=right)
  }

  # output
  if (is.null(output_file)) {
    if (output_format == 'html') {
      return(as.character(gt::as_raw_html(out)))
    } else if (output_format == 'latex') {
      return(as.character(gt::as_latex(out)))
    } else if (output_format %in% c('default', 'gt')) {
      return(out)
    }
  } else {
    gt::gtsave(out, output_file)
  }

}
