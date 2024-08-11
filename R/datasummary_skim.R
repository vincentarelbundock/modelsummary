#' Quick overview of numeric or categorical variables
#'
#' This function was inspired by the excellent `skimr` package for R.
#' See the Details and Examples sections below, and the vignettes on the
#' `modelsummary` website: 
#' * https://modelsummary.com/
#' * https://modelsummary.com/articles/datasummary.html
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @import data.table
#' @param type String. Variables to summarize: "all", "numeric", "categorical", "dataset"
#' @param by Character vector of grouping variables to compute statistics over.
#' @param fun_numeric Named list of funtions to apply to each column of `data`. If `fun_numeric` includes "Histogram" or "Density", inline plots are inserted. 
#'
#' @template citation
#' @template options
#' @examplesIf FALSE
#' dat <- mtcars
#' dat$vs <- as.logical(dat$vs)
#' dat$cyl <- as.factor(dat$cyl)
#' datasummary_skim(dat)
#' datasummary_skim(dat, type = "categorical")
#'
#' @export
datasummary_skim <- function(data,
                             output = 'default',
                             type   = 'all',
                             fmt    = 1,
                             title  = NULL,
                             notes  = NULL,
                             align  = NULL,
                             escape = TRUE,
                             by = NULL,
                             fun_numeric = list("Unique" = NUnique,
                                                "Missing Pct." = PercentMissing,
                                                "Mean" = Mean,
                                                "SD" = SD,
                                                "Min" = Min,
                                                "Median" = Median,
                                                "Max" = Max,
                                                "Histogram" = function(x) ""),
                             ...) {

  ## settings 
  settings_init(settings = list("function_called" = "datasummary_skim"))
  tmp <- sanitize_output(output) # before sanitize_escape
  output_format <- tmp$output_format
  output_factory <- tmp$output_factory
  output_file <- tmp$output_file
  sanitize_escape(escape) # after sanitize_output
  sanity_align(align)
  checkmate::assert_list(fun_numeric, min.len = 1, names = "unique")
  for (fun_numeric_element in fun_numeric) {
    checkmate::assert_function(fun_numeric_element)
  }
  checkmate::assert_data_frame(data)
  checkmate::assert_character(by, null.ok = TRUE)
  if (!is.null(by)) {
    checkmate::assert_true(all(by %in% colnames(data)))
  }

  dots <- list(...)
  if (isFALSE(dots[["histogram"]])) {
    fun_numeric[["Histogram"]] <- NULL
    insight::format_warning("The `histogram` argument is deprecated. Use `fun_numeric` instead.")
  }

  # in 2.0.0, histogram is a tinytable-specific option.
  if (!settings_equal("output_factory", c("tinytable", "dataframe"))) {
    insight::format_warning("Inline histograms in `datasummary_skim()` are only supported for tables produced by the `tinytable` backend.")
    fun_numeric[["Histogram"]] <- NULL
  }

  checkmate::assert_choice(type, c("all", "numeric", "categorical", "dataset"))

  # tables does not play well with tibbles
  data <- as.data.frame(data)

  if (type == "all" && !settings_equal("output_factory", "tinytable")) {
    insight::format_warning("`type='all'` is only supported for the `tinytable` backend. Set the `type` argument explicitly to suppress this warning.")
    type <- "numeric"
  }

  if (type == "all") {
    a <- tryCatch(datasummary_skim_numeric(data,
                                           output = "tinytable", fmt = fmt, by = by,
                                           title = title, notes = notes, align = align,
                                           escape = FALSE, fun_numeric = fun_numeric, ...),
                  error = function(e) e$message)
    b <- tryCatch(datasummary_skim_categorical(data,
                                               output = "tinytable", fmt = fmt,
                                               title = title, notes = notes, align = align,
                                               escape = FALSE, ...),
                  error = function(e) e$message)

    sanitize_output(output)

    data_list <- attr(a, "data_list")

    if (inherits(a, "tinytable") && inherits(b, "tinytable")) {
      out <- tinytable::rbind2(a, b, use_names = FALSE)
      out <- tinytable::format_tt(out, replace = "")
      out <- tinytable::style_tt(out, i = nrow(a) + 1, line = "t", line_size = .3)
      if (identical(output_format, "html")) {
          out <- tinytable::style_tt(out, i = nrow(a) + 1, bold = TRUE, line = "bt", line_color = "#d3d8dc")
      }
    } else if (!inherits(a, "tinytable") && !inherits(b, "tinytable")) {
      insight::format_error(a, b)
    } else if (inherits(a, "tinytable")) {
      out <- a
    } else if (inherits(b, "tinytable")) {
      out <- b
    }

    ofmt <- output_format
    if (isTRUE(ofmt %in% c("latex", "markdown", "html", "typst", "dataframe"))) {
      out@output <- ofmt
    }
    ofile <- output_file
    if (!is.null(ofile)) {
      tinytable::save_tt(out, output = ofile, overwrite = TRUE)
    }

    out <- tinytable::format_tt(out, escape = escape)

  } else if (type == "numeric") {
    out <- datasummary_skim_numeric(data,
      output = output, fmt = fmt, by = by,
      title = title, notes = notes, align = align,
      escape = escape, fun_numeric = fun_numeric, ...)
    sanitize_output(output)

    data_list <- attr(out, "data_list")

  } else if (type == "categorical") {
    out <- datasummary_skim_categorical(data,
      output = output, fmt = fmt,
      title = title, notes = notes, align = align,
      escape = escape, ...)
    sanitize_output(output)

  } else if (type == "dataset") {
    out <- datasummary_skim_dataset(data,
      output = output, title = title,
      notes = notes, align = align,
      escape = escape, ...)
    sanitize_output(output)
  }

  if (inherits(out, "tinytable")) {
    if ("Histogram" %in% out@names && !is.null(data_list)) {
      out <- tinytable::plot_tt(out, i = seq_along(data_list), j = "Histogram", fun = "histogram", data = data_list)
    }
    if ("Density" %in% out@names && !is.null(data_list)) {
      out <- tinytable::plot_tt(out, i = seq_along(data_list), j = "Density", fun = "density", data = data_list)
    }
  }

  if (!is.null(output_file)) {
    settings_rm()
    return(invisible(out))
  } else {
    if (output == "jupyter" || (output == "default" && settings_equal("output_default", "jupyter"))) {
      insight::check_if_installed("IRdisplay")
      return(invisible(IRdisplay::display_html(as.character(out))))
    }
    settings_rm()
    return(out)
  }

}

#' Internal function to skim whole datasets
#'
#' @noRd
datasummary_skim_dataset <- function(
  data,
  output,
  title,
  notes,
  align,
  escape,
  ...) {



  is.binary <- function(x) {
    tryCatch(length(unique(stats::na.omit(x))) == 2, error = function(e) FALSE, silent = TRUE)
  }
  rounding <- fmt_decimal(digits = 0)
  out <- c(
    Rows = rounding(nrow(data)),
    Columns = rounding(ncol(data)),
    # `# Binary` = rounding(sum(sapply(data, is.binary))),
    `# Character` = rounding(sum(sapply(data, is.character))),
    `# Factor` = rounding(sum(sapply(data, is.factor))),
    `# Logical` = rounding(sum(sapply(data, is.logical))),
    `# Numeric` = rounding(sum(sapply(data, is.numeric))),
    `% Missing` = rounding(mean(is.na(data) * 100))
  )
  out <- data.frame(names(out), out)
  out <- out[out[[2]] != "0" | out[[1]] == "% Missing", ]
  row.names(out) <- NULL
  colnames(out) <- c(" ", "  ")

  out <- datasummary_df(
    data = out,
    output = output,
    title = title,
    align = align,
    notes = notes,
    ...)

  return(out)

}


#' Internal function to skim numeric variables
#'
#' @noRd
datasummary_skim_numeric <- function(data,
                                     output,
                                     fmt,
                                     title,
                                     notes,
                                     align,
                                     escape,
                                     by = NULL,
                                     fun_numeric = NULL,
                                     ...) {

  # subset of numeric variables
  idx <- sapply(data, is.numeric)
  idx[colnames(data) %in% by] <- TRUE
  if (!any(idx)) insight::format_error('data contains no numeric variable.')
  dat_new <- data[, idx, drop = FALSE]

  # subset of non-NA variables
  idx <- sapply(dat_new, function(x) !all(is.na(x)))
  if (!any(idx)) insight::format_error('all numeric variables are completely missing.')
  dat <- dat_new[, idx, drop = FALSE]

  # too large
  if (ncol(dat) > 250) {
    insight::format_error("Cannot summarize more than 250 variables at a time.")
  }

  cols <- setdiff(colnames(dat), by)

  dat <- data.table::as.data.table(dat)

  funcs <- list(
      "Variable" = function(x) "",
      "Internal Data List" = function(x) list(x)
  )
  funcs <- c(funcs, fun_numeric)

  # Compute
  rows <- list()
  for (v in cols) {
    tmp <- dat[, lapply(funcs, function(funny) funny(variable)), 
               by = by,
               env = list("variable" = v)][
               , Variable := v]
    for (i in seq_along(tmp)) {
      class(tmp[[i]]) <- setdiff(class(tmp[[i]]), c("haven_labelled", "vctrs_vctr"))
    }
    rows <- c(rows, list(tmp))
  }
  rows <- data.table::rbindlist(rows)

  data_list <- rows[["Internal Data List"]]

  # labels
  for (col in colnames(data)) {
    lab <- attr(data[[col]], "label")
    if (!is.null(lab)) {
      rows[Variable == col, Variable := lab]
    }
  }
  
  rows[, Variable := dedup(Variable)]
  rows[, `Internal Data List` := NULL]
  idx <- unique(c("Variable", by, colnames(rows)))
  rows <- rows[, ..idx]
  data.table::setnames(rows, old = "Variable", new = " ")

  out <- datasummary_df(rows,
                        fmt = fmt,
                        output = output)


  attr(out, "data_list") <- data_list

  return(out)
}



#' Internal function to skim categorical variables
#'
#' @noRd
datasummary_skim_categorical <- function(
  data,
  output,
  fmt,
  title,
  notes,
  align,
  escape,
  ...) {

  dat_new <- data

  # pad colnames in case one is named Min, Max, Mean, or other function name
  # colnames(dat_new) <- paste0(colnames(dat_new), " ")

  drop_too_many_levels <- NULL
  drop_entirely_na <- NULL


  for (n in colnames(dat_new)) {

    # completely missing
    if (all(is.na(dat_new[[n]]))) {
      dat_new[[n]] <- NULL
      drop_entirely_na <- c(drop_entirely_na, n)
    }

    if (is.logical(dat_new[[n]]) |
        is.character(dat_new[[n]]) |
        is.factor(dat_new[[n]])) {

      # convert to factor and keep NAs as distinct level
      if (is.logical(dat_new[[n]]) | is.character(dat_new[[n]])) {
        dat_new[[n]] <- factor(dat_new[[n]], exclude = NULL)
      }

      # tables::tabular breaks on ""
      if (is.factor(dat_new[[n]]) && "" %in% levels(dat_new[[n]])) {
        idx <- levels(dat_new[[n]]) == ""
        levels(dat_new[[n]])[idx] <- " "
      }

      ## factors with too many levels
      if (is.factor(dat_new[[n]])) {
          if (length(levels(dat_new[[n]])) > 50) {
              dat_new[[n]] <- NULL
              drop_too_many_levels <- c(drop_too_many_levels, n)
          }
      }

    # discard non-factors
    } else {
      dat_new[[n]] <- NULL
    }

  }

  # too small
  if (ncol(dat_new) == 0) {
    stop('data contains no logical, character, or factor variable.')
  }

  # too large
  if (ncol(dat_new) > 50) {
    stop("Cannot summarize more than 50 variables at a time.")
  }

  if (!is.null(drop_too_many_levels)) {
    warning(sprintf("These variables were omitted because they include more than 50 levels: %s.", paste(drop_too_many_levels, collapse=", ")),
            call. = FALSE)
  }

  if (!is.null(drop_entirely_na)) {
    warning(sprintf("These variables were omitted because they are entirely missing: %s.", paste(drop_entirely_na, collapse=", ")),
            call. = FALSE)
  }

  pctformat <- sanitize_fmt(fmt)
  f <- All(dat_new, numeric = FALSE, factor = TRUE, logical = TRUE, character = TRUE) ~
       (N = 1) * Format() + (`%` = Percent()) * Format(pctformat())

  datasummary(
    formula = f,
    data = dat_new,
    output = output,
    title = title,
    align = align,
    notes = notes)

}



dedup <- function(x) {
  if (length(x) < 2) return(x)
  for (i in length(x):2) {
    if (x[i] == x[i - 1]) {
      x[i] <- NA
    }
  }
  if (is.character(x)) {
    x[is.na(x)] <- ""
  }
  return(x)
}
