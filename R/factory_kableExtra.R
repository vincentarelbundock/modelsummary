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



  if (is.null(output_format) || !output_format %in% c("latex", "markdown")) {
    output_format <- "html"
  }

  # kbl arguments
  valid <- c("x", "align", "caption", "format", "booktabs", "linesep",
             "format.args", "escape", "table.attr", "longtable", "valign",
             "position", "centering", "vline", "toprule", "bottomrule",
             "midrule", "caption.short", "table.envir") 
  arguments <- c(
    list(...),
    "caption"   = title,
    "format"    = output_format,
    "booktabs"  = TRUE,
    "linesep"   = "",
    "row.names" = NULL
  )

  # align
  if (!is.null(align)) {
    arguments[["align"]] <- align

    # if dcolumn, wrap model names in multicolumn to avoid math mode
    if (output_format == "latex" && 
        any(grepl("D\\{", align)) &&
        !"escape" %in% names(arguments)) {
      colnames(tab) <- paste0("\\multicolumn{1}{c}{", colnames(tab), "}")
      arguments$escape <- FALSE
    }
  }

  # don't print row.names
  row.names(tab) <- NULL

  # combine arguments
  arguments <- arguments[base::intersect(names(arguments), valid)]
  arguments <- c(list(tab), arguments)

  out <- do.call(kableExtra::kbl, arguments)

  # horizontal rule to separate coef/gof not supported in markdown
  # TODO: support HTML
  if (!is.null(hrule)) {
    if (output_format %in% 'latex') {
      for (pos in hrule) {
        out <- kableExtra::row_spec(out,
          row = pos - 1, 
          extra_latex_after = "\\midrule")
      }
    } else if (output_format %in% "html") {
      for (pos in hrule) {
        out <- kableExtra::row_spec(out,
          row = pos - 1,
          extra_css = "box-shadow: 0px 1px")
      }
    }
  }

  # user-supplied notes at the bottom of table
  if (!is.null(notes)) {
    # threeparttable only works with 1 note. But it creates a weird bug
    # when using coef_map and stars in Rmarkdown PDF output
    for (n in notes) {
      # otherwise stars_note breaks in PDF output under pdflatex
      if (output_format == "latex" && isTRUE(grepl(" < ", n))) {
        n <- gsub(" < ", " $<$ ", n)
        out <- kableExtra::add_footnote(out, label=n, notation='none',
                                        escape=FALSE)
      } else {
        out <- kableExtra::add_footnote(out, label=n, notation='none',
                                        escape=TRUE)
      }
    }
  }

  span <- attr(tab, 'span_kableExtra')
  if (!is.null(span)) {
    # add_header_above not supported in markdown
    if (output_format %in% c('latex', 'html')) {
      span <- rev(span) # correct vertical order
      for (s in span) {
        out <- kableExtra::add_header_above(out, s)
      }
    }
  }

  # styling (can be overriden manually by calling again)
  if (output_format %in% c("latex", "html")) {
    out <- kableExtra::kable_styling(out, full_width = FALSE)
  }

  # output
  if (is.null(output_file)) {
    return(out)
  } else {
    if (output_format == "markdown") {
      writeLines(paste(out, collapse="\n"), con=output_file)
    } else {
      kableExtra::save_kable(out, file = output_file)
    }
  }

}
