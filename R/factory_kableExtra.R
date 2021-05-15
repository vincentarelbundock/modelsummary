#' Internal function to build table with `kableExtra`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return kableExtra object
factory_kableExtra <- function(tab,
                               align = NULL,
                               hrule = NULL,
                               notes = NULL,
                               output_file = NULL,
                               output_format = 'kableExtra',
                               title = NULL,
                               ...) {

  # new variable "kable_format" because "kableExtra" and "html" both produce
  # html, but we need to distinguish the two.
  kable_format <- "html"
  if (!is.null(output_format)) {
    if (output_format %in% c("latex", "latex_tabular")) {
      kable_format <- "latex"
    } else if (output_format == "markdown") {
      kable_format <- "markdown"
    }
  }

  # don't print row.names
  row.names(tab) <- NULL

  # kbl arguments
  valid <- c("x", "align", "caption", "format", "booktabs", "linesep",
             "format.args", "escape", "table.attr", "longtable", "valign",
             "position", "centering", "vline", "toprule", "bottomrule",
             "midrule", "caption.short", "table.envir")

  arguments <- c(
    list(...),
    "caption"   = title,
    "format"    = kable_format,
    "booktabs"  = TRUE,
    "linesep"   = "",
    "row.names" = NULL
  )

  # align
  if (!is.null(align)) {
    arguments[["align"]] <- align

    # if dcolumn, wrap model names in multicolumn to avoid math mode
    if (kable_format %in% c("latex_tabular", "latex") &&
        any(grepl("D\\{", align)) &&
        !"escape" %in% names(arguments)) {
      colnames(tab) <- paste0("\\multicolumn{1}{c}{", colnames(tab), "}")
      arguments$escape <- FALSE
    }
  }

  # combine arguments
  arguments <- arguments[base::intersect(names(arguments), valid)]
  arguments <- c(list(tab), arguments)
  out <- do.call(kableExtra::kbl, arguments)

  # user-supplied notes at the bottom of table
  if (!is.null(notes) && output_format %in% c("kableExtra", "html", "latex", "markdown")) {
    # threeparttable only works with 1 note. But it creates a weird bug
    # when using coef_map and stars in Rmarkdown PDF output
    for (n in notes) {
      # otherwise stars_note breaks in PDF output under pdflatex
      if (kable_format == "latex" && isTRUE(grepl(" < ", n))) {
        n <- gsub(" < ", " $<$ ", n)
      }
      out <- kableExtra::add_footnote(out, label = n, notation = "none", escape = FALSE)
    }
  }

  span <- attr(tab, "span_kableExtra")
  if (!is.null(span) && output_format %in% c("kableExtra", "latex", "html")) {
    # add_header_above not supported in markdown
    span <- rev(span) # correct vertical order
    for (s in span) {
      out <- kableExtra::add_header_above(out, s)
    }
  }

  # theme
  theme_ms <- getOption("modelsummary_theme_kableExtra",
                        default = theme_ms_kableExtra)
  out <- theme_ms(out,
                  output_format = output_format,
                  hrule = hrule)

  # html & latex get a new class to use print.modelsummary_string
  if (output_format %in% c("latex", "latex_tabular", "html")) {
    class(out) <- c("modelsummary_string", class(out))
  }

  # output
  if (is.null(output_file)) {
    return(out)
  } else {
    if (output_format == "markdown") {
      writeLines(paste(out, collapse = "\n"), con = output_file)
    } else {
      kableExtra::save_kable(out, file = output_file)
    }
  }
}
