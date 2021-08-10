#' Internal function to build table with `kableExtra`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return kableExtra object
factory_kableExtra <- function(tab,
                               align = NULL,
                               hrule = NULL,
                               notes = NULL,
                               title = NULL,
                               ...) {

  # new variable "kable_format" because "kableExtra" and "html" both produce
  # html, but we need to distinguish the two.
  if (settings_equal("output_format", c("latex", "latex_tabular"))) {
    kable_format <- "latex"
  } else if (settings_equal("output_format", "markdown")) {
    kable_format <- "markdown"
  } else {
    kable_format <- "html"
  }

  ## don't print row.names
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


  ## never use `kableExtra`'s default escape
  arguments$escape <- FALSE

  ## siunitx in preamble
  if (settings_equal("format_numeric_latex", "siunitx")) {
      invisible(
        knitr::knit_meta_add(list(
          rmarkdown::latex_dependency("booktabs"))))
      invisible(knitr::knit_meta_add(list(
          rmarkdown::latex_dependency("siunitx", extra_lines = "\\newcolumntype{d}{S[input-symbols = ()]}")
      )))
  }

  ## align
  if (!is.null(align)) {
    for (i in seq_along(align)) {
      if (align[i] == "d") {
        if (settings_equal("output_format", c("latex", "latex_tabular"))) {
          ## protect strings from siunitx
          tab[[i]] <- ifelse(!grepl("[0-9]", tab[[i]]), sprintf("{%s}", tab[[i]]), tab[[i]]) 
        } else {
          tab[[i]] <- ifelse(grepl("[0-9]", tab[[i]]), sprintf("$%s$", tab[[i]]), tab[[i]])
        }
      }

    }
    if (any(grepl("d", align))) {
      ## protect column labels
      colnames(tab)[align == "d"] <- sprintf("{%s}", colnames(tab)[align == "d"])
    }
    arguments[["align"]] <- align
  }

  # combine arguments
  arguments <- arguments[base::intersect(names(arguments), valid)]
  arguments <- c(list(tab), arguments)
  out <- do.call(kableExtra::kbl, arguments)


  ## footnote arguments
  valid <- c("footnote_as_chunk", "escape", "threeparttable", "fixed_small_size", "symbol_manual", "title_format")
  arguments <- list(...)
  arguments <- arguments[base::intersect(names(arguments), valid)]

  ## kableExtra::footnote bug when adding multiple notes with threeparttable in LaTeX
  ## combine notes
  if (settings_equal("output_format", "latex") &&
      !is.null(notes) &&
      length(notes) > 1 &&
      "threeparttable" %in% names(arguments) &&
      isTRUE(arguments[["threeparttable"]])) {
      notes <- paste(notes, collapse = " ")
  }

  ## user-supplied notes at the bottom of table
  if (!is.null(notes)) {
    ## kableExtra::footnote does not support markdown
    ## kableExtra::add_footnote does not support longtable
    if (settings_equal("output_format", c("kableExtra", "html", "latex"))) {
      ## threeparttable only works with 1 note. But it creates a weird bug
      ## when using coef_map and stars in Rmarkdown PDF output
      for (n in notes) {
        ## otherwise stars_note breaks in PDF output under pdflatex
        if (kable_format == "latex" && isTRUE(grepl(" < ", n))) {
          n <- gsub(" < ", " $<$ ", n)
        }
        arguments[["general"]] <- n
        arguments[["general_title"]] <- ""
        arguments[["kable_input"]] <- out
        arguments[["escape"]] <- FALSE
        out <- do.call(kableExtra::footnote, arguments)
      }
    } else if (settings_equal("output_format", "markdown")) {
      for (n in notes) {
        out <- kableExtra::add_footnote(out, label = n, notation = "none", escape = FALSE)
      }
    }
  }

  span <- attr(tab, "span_kableExtra")
  if (!is.null(span) && settings_equal("output_format", c("kableExtra", "latex", "html"))) {
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
                  output_format = settings_get("output_format"),
                  hrule = hrule)


  # html & latex get a new class to use print.modelsummary_string
  if (settings_equal("output_format", c("latex", "latex_tabular", "html"))) {
    class(out) <- c("modelsummary_string", class(out))
  }

  # output
  if (is.null(settings_get("output_file"))) {
    return(out)
  } else {
    if (settings_equal("output_format", "markdown")) {
      writeLines(paste(out, collapse = "\n"), con = settings_get("output_file"))
    } else {
      kableExtra::save_kable(out, file = settings_get("output_file"))
    }
  }
}
