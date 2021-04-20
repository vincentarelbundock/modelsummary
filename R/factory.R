#' Factory to create tables in different output formats using standardized
#' inputs.
#'
#' @param tab table body (data.frame)
#' @param hrule position of horizontal rules (integer vector)
#' @noRd
#' @inheritParams modelsummary
#' @inheritParams datasummary
factory <- function(tab,
                    align = NULL,
                    fmt = 3,
                    hrule = NULL,
                    notes = NULL,
                    output = NULL,
                    title = NULL,
                    add_rows = NULL,
                    add_columns = NULL,
                    ...) {


  # sanity check functions are hosted in R/sanity_checks.R
  sanity_output(output)
  sanity_title(title)
  sanity_notes(notes)

  # parse output
  output_list <- sanitize_output(output)

  if (output_list$output_factory == 'gt') {
    f <- factory_gt
  } else if (output_list$output_factory == 'kableExtra') {
    f <- factory_kableExtra
  } else if (output_list$output_factory == 'flextable') {
    f <- factory_flextable
  } else if (output_list$output_factory == 'huxtable') {
    f <- factory_huxtable
  } else if (output_list$output_factory == 'dataframe') {
    f <- factory_dataframe
  }

  # flat header if necessary
  flat_header <- attr(tab, 'header_sparse_flat')
  if (!is.null(flat_header)) {
    flat_factories <- c('flextable', 'huxtable', 'dataframe')
    flat_formats <- c('markdown', 'word', 'powerpoint')
    if ((output_list$output_factory %in% flat_factories) ||
      output_list$output_format %in% flat_formats) {
      attr(tab, "header_bottom") <- colnames(tab)

      # datasummary_balance with dinm produces more cols than flat_header
      for (i in seq_along(flat_header)) {
        colnames(tab)[i] <- flat_header[i]
      }

    }
  }

  # de-duplicate columns with whitespace
  colnames(tab) <- pad(colnames(tab))

  # add_columns
  if (!is.null(add_columns)) {

    # sanity check
    checkmate::assert_data_frame(add_columns, min.cols = 1, min.rows = 1)

    pos <- attr(add_columns, 'position')

    # convert to numeric
    for (i in seq_along(add_columns)) {
      if (is.numeric(add_columns[[i]])) {
        add_columns[[i]] <- rounding(add_columns[[i]], fmt)
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
    if (output_list$output_format == "dataframe") {
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
    for (i in 1:ncol(add_rows)) {
      if (is.numeric(add_rows[[i]])) {
        add_rows[[i]] <- rounding(add_rows[[i]], fmt)
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

  # sanity align: after add_columns
  sanity_align(align, tab)

  # build table
  out <- f(tab,
    align = align,
    hrule = hrule,
    notes = notes,
    output_file = output_list$output_file,
    output_format = output_list$output_format,
    title = title,
    ...)

  if (output == "jupyter" ||
      (output == "default" && getOption("modelsummary_default", "kableExtra") == "jupyter")) {
    assert_dependency("IRdisplay")
    IRdisplay::display_html(as.character(out))
  } else {
    return(out)
  }
}
