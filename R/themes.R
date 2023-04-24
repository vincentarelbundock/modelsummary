theme_ms_kableExtra <- function(tab,
                                output_format,
                                hrule = NULL,
                                hgroup = NULL,
                                hindent = NULL,
                                ...) {

  dots <- list(...)

  if (!output_format %in% c("markdown", "latex_tabular")) {
    out <- kableExtra::kable_styling(tab, full_width = FALSE)
  } else {
    out <- tab
  }

  # kableExtra quirk: latex hrule before hgroup
  if (!is.null(hrule) && output_format %in% c("latex", "latex_tabular")) {
    for (pos in hrule) {
      out <- kableExtra::row_spec(out,
        row = pos - 1,
        extra_latex_after = "\\midrule"
      )
    }
  }

  # groups are not supported by kableExtra in markdown output
  if (!is.null(hgroup) && !output_format %in% "markdown") {
    for (i in seq_along(hgroup)) {
      args <- list(
        kable_input = out,
        bold = FALSE,
        italic = TRUE,
        indent = TRUE,
        hline_after = FALSE,
        extra_latex_after = "\\midrule ",
        latex_gap_space = "0.5em",
        group_label = names(hgroup[i]),
        start_row = hgroup[[i]][1],
        end_row = hgroup[[i]][2],
        escape = FALSE)

      # user-specified arguments override default themes
      dots <- list(...)
      for (n in names(dots)) {
        args[[n]] <- dots[[n]]
      }
      valid <- names(formals(kableExtra::group_rows))
      args <- args[names(args) %in% valid]

      out <- do.call(kableExtra::group_rows, args)
    }
  }

  # kableExtra quirk: latex hrule before hgroup
  if (!is.null(hrule) && output_format %in% c("kableExtra", "html")) {
    for (pos in hrule) {
      out <- kableExtra::row_spec(out,
        row = pos - 1,
        extra_css = "box-shadow: 0px 1.5px"
      )
    }
  }

  # indent to match group indents
  if (isTRUE(checkmate::check_list(hindent, min.len = 1))) {
    fun <- utils::getFromNamespace("add_indent", "kableExtra")
    idx <- unlist(lapply(hindent, function(x) x[1]:x[2]))
    out <- fun(out, positions = idx)
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
