#' Factory to create tables in different output formats using standardized
#' inputs.
#'
#' @param tab table body (data.frame)
#' @param hrule position of horizontal rules (integer vector)
#' @noRd
#' @inheritParams datasummary
#' @inheritParams modelsummary
factory <- function(tab,
                    align = NULL,
                    fmt = 3,
                    hrule = NULL,
                    notes = NULL,
                    output = NULL,
                    title = NULL,
                    add_rows = NULL,
                    add_columns = NULL,
                    escape = TRUE,
                    output_factory = "tinytable",
                    output_format = "tinytable",
                    output_file = NULL,
                    ...) {


  # sanity check functions are hosted in R/sanity_checks.R
  sanity_title(title, ...)
  sanity_notes(notes)

  # parse output
  if (isTRUE(output == "data.frame")) { # internal calls from datasummary_skim()
    factory_fun <- factory_dataframe
  } else if (output_factory == "gt") {
    factory_fun <- factory_gt
  } else if (output_factory == "tinytable") {
    factory_fun <- factory_tinytable
  } else if (output_factory == "kableExtra") {
    factory_fun <- factory_kableExtra
  } else if (output_factory == "flextable") {
    factory_fun <- factory_flextable
  } else if (output_factory == "huxtable") {
    factory_fun <- factory_huxtable
  } else if (output_factory == "DT") {
    factory_fun <- factory_DT
  } else if (output_factory == "dataframe") {
    factory_fun <- factory_dataframe
  } else if (output_factory == "modelsummary") {
    factory_fun <- factory_markdown
  } else if (output_factory == "typst") {
    factory_fun <- factory_typst
  }

  # flat header if necessary
  flat_header <- attr(tab, 'header_sparse_flat')
  if (!is.null(flat_header)) {
    flat_factories <- c('huxtable', 'dataframe', 'typst')
    flat_formats <- c('markdown', 'word', 'powerpoint', 'typst')
    if (output_factory %in% flat_factories ||
        output_format %in% flat_formats) {
        attr(tab, "header_bottom") <- colnames(tab)

      # datasummary_balance with dinm produces more cols than flat_header
      for (i in seq_along(flat_header)) {
        colnames(tab)[i] <- flat_header[i]
      }

    }
  }

  # de-duplicate columns with whitespace
  colnames(tab) <- pad(colnames(tab), output_format = output_format)

  # add_columns
  if (!is.null(add_columns)) {

    # sanity check
    checkmate::assert_data_frame(add_columns, min.cols = 1, min.rows = 1)

    pos <- attr(add_columns, 'position')

    # `fmt`: modelsummary() supplies a list with `fmt` default, but not other functions
    if (isTRUE(checkmate::check_list(fmt))) {
      fmt <- fmt[["fmt"]]
    }

    fun <- sanitize_fmt(fmt)
    for (i in seq_along(add_columns)) {
      if (is.numeric(add_columns[[i]])) {
        add_columns[[i]] <- fun(add_columns[[i]])
      } else {
        add_columns[[i]] <- as.character(add_columns[[i]])
      }
    }

    # pad with empty cells if insufficient rows
    nrow_gap <- nrow(tab) - nrow(add_columns)
    if (nrow_gap > 0) {
      tmp <- matrix('', ncol = ncol(add_columns), nrow = nrow_gap)
      tmp <- data.frame(tmp)
      colnames(tmp) <- colnames(add_columns)
      add_columns <- bind_rows(add_columns, tmp)
    }

    # append
    for (i in seq_along(add_columns)) {
      if (!is.null(pos) && !is.na(pos[i])) {
        lef <- tab[, -c(pos[i]:ncol(tab)), drop = FALSE]
        rig <- tab[, c(pos[i]:ncol(tab)), drop = FALSE]
        tab <- bind_cols(lef, add_columns[i], rig)
      } else {
        tab <- bind_cols(tab, add_columns[i])
      }
    }

    # pad headers
    ks <- attr(tab, 'span_kableExtra')
    if (!is.null(ks)) {
      for (i in seq_along(ks)) {
        # 5 spaces is a hack
        ks[[i]] <- c(ks[[i]], '     ' = ncol(add_columns))
      }
      attr(tab, 'span_kableExtra') <- ks
    }
  }

  # add_rows
  if (!is.null(add_rows)) {

    # data.frame includes metadata columns
    if (identical(output_format, "dataframe")) {
      # only for modelsummary, not for datasummary

      if (all(c("term", "statistic") %in% colnames(tab))) {
        add_rows$part <- "manual"
        add_rows$statistic <- ""
        add_rows <- add_rows[, colnames(tab)]
      }

    }

    # sanity check
    checkmate::assert_data_frame(add_rows, min.rows = 1, ncols = ncol(tab))

    colnames(add_rows) <- colnames(tab)
    pos <- attr(add_rows, 'position')

    # convert to character
    fun <- sanitize_fmt(fmt)
    for (i in 1:ncol(add_rows)) {
      if (is.numeric(add_rows[[i]])) {
        add_rows[[i]] <- fun(add_rows[[i]])
      } else {
        add_rows[[i]] <- as.character(add_rows[[i]])
      }
    }

    # append
    for (i in 1:nrow(add_rows)) {
      # append
      if (!is.null(pos) && !is.na(pos[i])) {
        top <- tab[-c(pos[i]:nrow(tab)), , drop = FALSE]
        bot <- tab[c(pos[i]:nrow(tab)), , drop = FALSE]
        tab <- bind_rows(top, add_rows[i, , drop = FALSE], bot)
      } else {
        tab <- bind_rows(tab, add_rows[i, , drop = FALSE])
      }
    }
  }

  ## align: sanity must be checked after add_columns
  if (is.null(align)) {
    align <- strrep("l", ncol(tab))
  } else {
    checkmate::assert_true(nchar(align) == ncol(tab))
  }
  align <- strsplit(align, "")[[1]]

  # dot align with unicode spaces (latex has its own mechanism)
  if (!output_format %in% c("latex", "tinytable")) {
    align_d <- grep("d", align)
    for (i in align_d) {
      tab[[i]] <- pad(tab[[i]], style = "character", output_format = output_format)
    }
    align[align == "d"] <- "c"
  }

  ## build table
  out <- factory_fun(tab,
    align = align,
    hrule = hrule,
    notes = notes,
    title = title,
    escape = escape,
    output_format = output_format,
    output_file = output_file,
    ...)


  if (output == "jupyter" ||
      (output == "default" && settings_equal("output_default", "jupyter"))) {
    insight::check_if_installed("IRdisplay")
    IRdisplay::display_html(as.character(out))
  } else {
    return(out)
  }
}
