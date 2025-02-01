#' Internal function to build table with `tinytable`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return tinytable object
factory_tinytable <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              hgroup = NULL,
                              hindent = FALSE,
                              notes = NULL,
                              title = NULL,
                              escape = TRUE,
                              output_format = "tinytable",
                              output_file = NULL,
                              gof_idx = NULL,
                              ...) {
  insight::check_if_installed("tinytable")

  span_list <- get_span_kableExtra(tab)

  # colnames with or without spans: before escape and for all span/no-span
  if (is.null(span_list)) {
    if (!is.null(colnames(tab))) {
      colnames(tab) <- gsub("\\|{4}", " / ", colnames(tab))
    }
  } else {
    colnames(tab) <- attr(span_list, "column_names")
  }

  # escape everything except \\num{} in LaTeX
  if (isTRUE(escape) && isTRUE(output_format %in% c("latex", "latex_tabular", "html", "typst", "tinytable"))) {
    of <- if (output_format == "latex_tabular") "latex" else output_format
    tmp <- escape_everything(
      tab = tab,
      output_format = of,
      span_list = span_list,
      title = title,
      notes = notes
    )
    tab <- tmp$tab
    title <- tmp$title
    notes <- tmp$notes
    span_list <- tmp$span_list
  }

  # create tables with combined arguments
  arguments <- list(caption = title)
  if (length(notes) > 1) {
    arguments$notes <- as.list(notes)
  } else {
    arguments$notes <- notes
  }
  arguments <- c(arguments, list(...))
  arguments <- arguments[base::intersect(names(arguments), c("x", "theme", "placement", "width", "caption", "align", "notes"))]
  arguments <- c(list(tab), arguments)
  out <- do.call(tinytable::tt, arguments)


  # align: other factories require a vector of "c", "l", "r", etc.
  # before span because those should be centered
  if (!is.null(align)) {
    for (idx in seq_along(tab)) {
      out <- tinytable::style_tt(out, j = idx, align = align[idx])
    }
  }

  # span: compute
  # after align, otherwise span alignment is overridden
  if (!is.null(span_list)) {
    for (i in seq_along(span_list)) {
      sp <- cumsum(span_list[[i]])
      sp <- as.list(sp)
      sp[[1]] <- 1:sp[[1]]
      sp[2:length(sp)] <- lapply(2:length(sp), function(k) (max(sp[[k - 1]]) + 1):sp[[k]])
      sp <- sp[trimws(names(sp)) != ""]
      out <- tinytable::group_tt(out, j = sp)
      out <- tinytable::style_tt(out, i = -i, align = "c")
    }
  }

  if (!is.null(hrule)) {
    for (h in hrule) {
      out <- tinytable::style_tt(out, i = h - 1, line = "b", line_width = .05)
    }
  }

  if (!is.null(hgroup) && length(hgroup) > 0) {
    hg <- sapply(hgroup, min)
    names(hg) <- names(hgroup)
    hg <- as.list(hg)
    out <- tinytable::group_tt(out, i = hg)
  }

  if ("d" %in% align && !is.null(gof_idx)) {
    idx <- paste(match("d", align), collapse = ",")
    inn <- sprintf("cell{%s-%s}{%s}={guard,halign=c},", gof_idx + 1, nrow(out) + out@nhead, idx)
    out <- tinytable::style_tt(out, tabularray_inner = inn)
  }

  # write to file
  if (!is.null(output_file)) {
    tinytable::save_tt(out, output = output_file, overwrite = TRUE)
    return(invisible())
  }

  # change output format in the S4 object, but return a `tinytable` for when we
  # post-process it with `plot_tt()` in `datasummary_skim()`
  if (output_format %in% c("latex", "typst", "html", "markdown")) {
    out@output <- output_format
  } else if (output_format %in% "latex_tabular") {
    out@output <- "latex"
    out <- tinytable::theme_tt(out, "tabular")
  }

  return(invisible(out))
}



escape_everything <- function(tab, output_format, span_list, title, notes) {
  # body: do not escape siunitx \num{}
  for (col in seq_len(ncol(tab))) {
    tab[[col]] <- ifelse(
      grepl("\\\\num\\{", tab[[col]]),
      tab[[col]],
      tinytable::format_tt(tab[[col]], escape = output_format)
    )
  }

  for (i in seq_along(span_list)) {
    names(span_list[[i]]) <- tinytable::format_tt(names(span_list[[i]]), escape = output_format)
  }

  if (!is.null(colnames(tab))) {
    colnames(tab) <- tinytable::format_tt(colnames(tab), escape = output_format)
  }

  for (i in seq_along(notes)) {
    # hack: avoid escaping stars notes with \num{} in LaTeX
    flag <- !identical(output_format, "latex") || !grepl("\\\\num\\{", notes[[i]])
    if (flag) {
      notes[[i]] <- tinytable::format_tt(notes[[i]], escape = output_format)
    }
  }

  if (isTRUE(checkmate::check_string(title))) {
    title <- tinytable::format_tt(title, escape = output_format)
  }

  out <- list(tab = tab, title = title, notes = notes, span_list = span_list)
  return(out)
}
