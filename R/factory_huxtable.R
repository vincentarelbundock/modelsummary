#' Internal function to build table with `huxtable`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return huxtable object
factory_huxtable <- function(tab,
                             align = NULL,
                             hrule = NULL,
                             notes = NULL,
                             title = NULL,
                             output_format = "huxtable",
                             output_file = NULL,
                             ...) {


  insight::check_if_installed("huxtable")

  colnames(tab) <- gsub("\\|\\|\\|\\|", " / ", colnames(tab))

  ## warning on align
  # if (!is.null(align)) {
  # warning('The `align` argument is not supported yet for huxtable objects. Please file a request or (even better) a code submission on Github if you need that feature.')
  # }

  # huxtable object with header
  out <- huxtable::hux(tab, add_colnames = TRUE)

  # title
  if (!is.null(title)) {
    out <- huxtable::set_caption(out, title)
  }

  # user-supplied notes at the bottom of table
  if (!is.null(notes)) {
    for (n in notes) {
      out <- huxtable::add_footnote(out, text = n)
    }
  }

  # theme
  theme_ms <- getOption("modelsummary_theme_huxtable",
                        default = theme_ms_huxtable)
  out <- theme_ms(out, hrule = hrule)

  # output
  output_file <- output_file
  if (is.null(output_file)) {
    return(out)
  } else {
    if (identical(output_format, "word")) {
      huxtable::quick_docx(out, file = output_file, open = FALSE)
    } else if (identical(output_format, "powerpoint")) {
      huxtable::quick_pptx(out, file = output_file, open = FALSE)
    } else if (identical(output_format, "html")) {
      huxtable::quick_html(out, file = output_file, open = FALSE)
    } else if (identical(output_format, "rtf")) {
      huxtable::quick_rtf(out, file = output_file, open = FALSE)
    } else if (identical(output_format, "latex")) {
      huxtable::quick_latex(out, file = output_file, open = FALSE)
    }
  }

}
