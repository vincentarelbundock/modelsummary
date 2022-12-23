theme_ms_kableExtra <- function(tab,
                                output_format,
                                hrule = NULL,
                                hgroup = NULL,
                                ...) {

  if (!output_format %in% c("markdown", "latex_tabular")) {
    out <- kableExtra::kable_styling(tab, full_width = FALSE)
  } else {
    out <- tab
  }

  # horizontal rule to separate coef/gof
  # not supported in markdown, and omitted from latex_tabular
  # row_spec does not play well with group_rows
  if (!is.null(hrule) && is.null(hgroup)) {
    if (output_format %in% c("latex", "latex_tabular")) {
      for (pos in hrule) {
        out <- kableExtra::row_spec(out,
          row = pos - 1,
          extra_latex_after = "\\midrule"
        )
      }
    } else if (output_format %in% c("kableExtra", "html")) {
      for (pos in hrule) {
        out <- kableExtra::row_spec(out,
          row = pos - 1,
          extra_css = "box-shadow: 0px 1px"
        )
      }
    }
  }

  if (!is.null(hgroup)) {
    for (i in seq_along(hgroup)) {
      out <- kableExtra::group_rows(
                out,
                group_label = names(hgroup)[i],
                start_row = hgroup[[i]][1],
                end_row = hgroup[[i]][2] )
    }
  }

  return(out)
}


theme_ms_gt <- function(tab,
                        output_format,
                        hrule = NULL,
                        hgroup = NULL,
                        ...) {
  out <- tab
  if (!is.null(hrule)) { # check if there are >0 GOF
    for (pos in hrule) {
      out <- gt::tab_style(
        out,
        style = gt::cell_borders(sides = "bottom", color = "#000000"),
        locations = gt::cells_body(rows = (pos - 1))
      )
    }
  }

  if (!is.null(hgroup)) {

  }

    # group rows by panel: gt
    hgroup <- rev(hgroup)
    for (i in seq_along(hgroup)) {
      out <- gt::tab_row_group(
        out,
        label = names(hgroup)[i],
        id = names(hgroup)[i],
        rows = hgroup[[i]][1]:hgroup[[i]][2])
    }

  return(out)
}


theme_ms_flextable <- function(tab,
                               output_format,
                               hrule,
                               ...) {
  out <- tab
  # horizontal rule to separate coef/gof
  if (!is.null(hrule)) {
    for (pos in hrule) {
      out <- flextable::border(out,
        i = pos,
        border.top = officer::fp_border()
      )
    }
  }
  return(out)
}


theme_ms_huxtable <- function(tab,
                              output_format,
                              hrule,
                              ...) {
  out <- tab

  # horizontal rules
  out <- huxtable::set_bottom_border(
    out,
    row = 1,
    col = 1:ncol(out),
    value = 1
  )
  out <- huxtable::set_top_border(
    out,
    row = 1,
    col = 1:ncol(out),
    value = 1
  )
  out <- huxtable::set_bottom_border(
    out,
    row = nrow(out),
    col = 1:ncol(out),
    value = 1
  )

  if (!is.null(hrule)) {
    for (pos in hrule) {
      out <- huxtable::set_bottom_border(
        out,
        row = pos,
        col = 1:ncol(out),
        value = 1
      )
    }
  }

  return(out)
}
