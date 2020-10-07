#' Difference in means using `estimatr`
#'
#' @keywords internal
DinM <- function(lhs, rhs, data, fmt, statistic) {

  assert_dependency("estimatr")

  if (!"clusters" %in% colnames(data))
      clusters <- NULL
  if (!"weights" %in% colnames(data))
      weights <- NULL
  if (!"blocks" %in% colnames(data))
      blocks <- NULL

  # needed for names with spaces
  data[["condition_variable_placeholder"]] <- data[[rhs]]
  data[["outcome_variable_placeholder"]] <- data[[lhs]]

  out <- estimatr::difference_in_means(
    outcome_variable_placeholder ~ condition_variable_placeholder,
    data = data, blocks = blocks, clusters = clusters, weights = weights)

  out <- estimatr::tidy(out)

  out <- out[, c("estimate", statistic), drop=FALSE]
  out[[1]] <- rounding(out[[1]], fmt)
  out[[2]] <- rounding(out[[2]], fmt)
  out$variable <- lhs

  if (statistic == "std.error") {
    colnames(out) <- c("Diff. in Means", "Std. Error", " ")
  } else if (statistic == "p.value") {
    colnames(out) <- c("Diff. in Means", "p", " ")
  } else {
    colnames(out) <- c("Diff. in Means", statistic, " ")
  }
  out

}

#' Balance table: Summary statistics for different subsets of the data (e.g.,
#' control and treatment groups)
#'
#' @param formula a one-sided formula with the "condition" or "column" variable
#'   on the right-hand side.
#' @param data A data.frame (or tibble). If this data includes columns called
#'   "blocks", "clusters", and/or "weights", the 'estimatr' package will
#'   consider them when calculating the difference in means.
#' @param dinm TRUE calculates a difference in means with uncertainty
#'   estimates. This option is only available if the `estimatr` package is
#'   installed. If `data` includes columns named "blocks", "clusters", or
#'   "weights", this information will be taken into account automatically by
#'   `estimatr::difference_in_means`.
#' @param dinm_statistic string: "std.error" or "p.value"
#' @inheritParams modelsummary
#' @inheritParams datasummary
#' @export
#' @examples
#' \dontrun{
#' datasummary_balance(~am, mtcars)
#' }
#' @keywords internal
datasummary_balance <- function(formula,
                                data,
                                output = 'default',
                                fmt = 1,
                                title = NULL,
                                notes = NULL,
                                align = NULL,
                                add_columns = NULL,
                                add_rows = NULL,
                                dinm = TRUE,
                                dinm_statistic = 'std.error',
                                ...) {

  # sanity checks
  sanity_output(output)
  sanity_ds_right_handed_formula(formula)
  checkmate::assert_formula(formula)
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 1)
  checkmate::assert_flag(dinm)
  checkmate::assert_string(dinm_statistic, pattern="^std.error$|^p.value$")

  # tables::tabular does not play well with tibbles
  data <- as.data.frame(data)

  # categorical data must be factor
  for (n in colnames(data)) {
    if (is.character(data[[n]]) || is.logical(data[[n]])) {
      data[[n]] <- as.factor(data[[n]])
    }
  }

  # rhs condition variable
  rhs <- labels(stats::terms(formula))

  if (!rhs %in% colnames(data)) {
    stop('Variable ', rhs, ' must be in data.')
  }

  data <- data[!is.na(data[[rhs]]), , drop = FALSE]

  if (length(unique(data[[rhs]])) > 10) {
    stop(sprintf('Each value of the `%s` variable will create two separate
                 columns. This variable has too many unique values, so the
                 table would be too wide to be readable.', rhs))
  }

  # nobs in column spans via factor levels
  lev <- table(data[[rhs]])
  lev <- paste0(names(lev), " (N=", lev, ")")
  levels(data[[rhs]]) <- lev

  # exclude otherwise All() makes them appear as rows 
  idx <- setdiff(colnames(data), 
                 c(rhs, 'clusters', 'blocks', 'weights'))
  data_norhs <- data[, idx, drop = FALSE]

  # 3-parts table: numeric + dinm / factor
  any_numeric <- any(sapply(data_norhs, is.numeric))
  any_factor <- any(sapply(data_norhs, is.factor))

  if (any_numeric) {
    f_num <- sprintf(
      'All(data_norhs) ~ 
      Factor(%s) * (Mean + Heading("Std. Dev.") * SD) * Arguments(fmt=fmt)',
      rhs)
    f_num <- formula(f_num)
    tab_num <- datasummary(f_num, data=data, output="data.frame")
    colnames(tab_num) <- pad(attr(tab_num, "header_bottom"))
  }

  if (any_factor) {

    # hack: `tables::tabular` produces different # of cols with a single or
    # multiple factors. Make sure there are multiple.
    data$badfactordropthis <- factor(c("badfactordropthis1", rep("badfactordropthis2", nrow(data)-1)))
    data_norhs$badfactordropthis <- factor(c("badfactordropthis1", rep("badfactordropthis2", nrow(data_norhs)-1)))

    pctformat = function(x) sprintf("%.1f", x)
    f_fac <- sprintf(
      'All(data_norhs, factor=TRUE, numeric=FALSE) ~ 
       Factor(%s) * (Heading("N")*1 * Format(digits=0) + 
       Heading("%%") * Percent() * Format(pctformat()))',
       rhs)
    if (any_numeric) {
      f_fac <- gsub('\\"\\%\\"', '\\"Std. Dev.\\"', f_fac)
      f_fac <- gsub('\\"N\\"', '\\"Mean\\"', f_fac)
    }
    tab_fac <- datasummary(
      formula(f_fac), data=data, output="data.frame")

    colnames(tab_fac) <- pad(attr(tab_fac, "header_bottom"))

    idx <- !grepl("^badfactordropthis\\d$", tab_fac[[2]])
    tab_fac <- tab_fac[idx, , drop=FALSE]

    # hack
    data_norhs$badfactordropthis <- data$badfactordropthis <- NULL

  }

  if (any_numeric && any_factor) {

    # header compatibility + new header
    header <- tab_fac[1, , drop=FALSE]
    cols <- trimws(colnames(header)) # we padded colnames above
    for (i in seq_along(header)) {
      header[1, i] <- ifelse(!cols[i] %in% c("Mean", "Std. Dev."), "", header[1, i])
      header[1, i] <- ifelse(cols[i] == "Mean", "N", header[1, i])
      header[1, i] <- ifelse(cols[i] == "Std. Dev.", "%", header[1, i])
    }
    tab_fac <- rbind(header, tab_fac)

    # bind tables and reorder columns (factor is always widest)
    tab <- dplyr::bind_rows(tab_num, tab_fac)
    tab <- tab[, colnames(tab_fac)]

    attr(tab, "stub_width") <- attr(tab_fac, "stub_width")
    attr(tab, "span_gt") <- attr(tab_fac, "span_gt")
    attr(tab, "span_kableExtra") <- attr(tab_fac, "span_kableExtra")
    attr(tab, "header_sparse_flat") <- attr(tab_fac, "header_sparse_flat")
    colnames(tab) <- gsub(".*\\) ", "", colnames(tab))

  } else if (any_numeric) {
    tab <- tab_num

  } else if (any_factor) {
    tab <- tab_fac

  }

  # difference in means
  if (!any_numeric) {
    dinm <- FALSE
  }

  if (dinm && isFALSE(check_dependency("estimatr"))) {
    dinm <- FALSE
    warning("Please install the `estimatr` package or set `dinm=FALSE` to
             suppress this warning.")
  }

  if (dinm && (length(unique(data[[rhs]])) > 2)) {
    dinm <- FALSE
    warning("The difference in means can only be calculate with two groups in
            the right-hand side variable. Set `dinm=FALSE` to suppress this
            warning.")
  }

  if (dinm) {
    numeric_variables <- colnames(data_norhs)[sapply(data_norhs, is.numeric)]
    tmp <- lapply(numeric_variables,
                  function(lhs) DinM(lhs=lhs, 
                                     rhs=rhs, 
                                     data=data, 
                                     fmt=fmt, 
                                     statistic=dinm_statistic))
    tmp <- do.call("rbind", tmp)
    tab <- dplyr::left_join(tab, tmp, by=" ")

    # post-DinM, pad the span if the table has new columns
    skE <- attr(tab, "span_kableExtra")
    for (i in seq_along(skE)) {
      skE[[i]] <- c(skE[[i]], rep("    ", ncol(tab) - sum(skE[[i]])))
    }
    attr(tab, "span_kableExtra") <- skE

    header_sparse_flat <- attr(tab, "header_sparse_flat")
    if (ncol(tab) > length(header_sparse_flat)) {
      attr(tab, "header_sparse_flat") <- c(header_sparse_flat, colnames(tab)[-c(1:length(header_sparse_flat))])
    }
    attr(tab, "header_sparse_flat") <- gsub(" \\(N=\\d+\\)", "", attr(tab, "header_sparse_flat"))
    attr(tab, "header_sparse_flat") <- pad(attr(tab, "header_sparse_flat"))

  }

  tab[is.na(tab)] <- ""

  if (any_numeric && any_factor) {
    hrule <- nrow(tab_num) + 1
  } else {
    hrule <- NULL
  }

  # make table
  factory(
    tab,
    align = align,
    hrule = hrule,
    notes = notes,
    fmt = fmt,
    output = output,
    add_rows = add_rows,
    add_columns = add_columns,
    title = title,
    ...)

}
