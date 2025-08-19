get_span_cbind <- function(models, shape) {
  if (!isTRUE(shape == "cbind")) {
    out <- list(models = models, shape = shape, span_cbind = NULL)
    return(out)
  }

  flag <- is.list(models) &&
    all(sapply(models, function(x) is.list(x))) &&
    !is.null(names(models))
  msg <- "With `shape='cbind', `models` must be a named list of lists of models."
  if (!flag) insight::format_error(msg)

  # spans
  model_names <- rep(names(models), sapply(models, length))
  model_indices <- seq_along(model_names)
  indices_list <- split(model_indices, model_names)
  final_indices <- lapply(indices_list, function(x) range(x))
  spans <- lapply(final_indices, function(x) x + 1) # stub in normal regression table

  # models
  # after spans
  if (settings_equal("output_factory", "tinytable")) {
    models <- do.call(c, unname(models))
  } else if (settings_equal("output_factory", "flextable")) {
    # For flextable, use unname() so spanning headers work properly
    models <- do.call(c, unname(models))
  } else {
    models <- do.call(c, models)
  }

  out <- list(
    models = models,
    shape = NULL,
    span_cbind = spans
  )

  return(out)
}


set_span_cbind <- function(tab, span_cbind) {
  out <- tab
  if (!is.null(span_cbind) && inherits(tab, "tinytable")) {
    out <- tinytable::group_tt(out, j = span_cbind)
  } else if (!is.null(span_cbind) && inherits(tab, "flextable")) {
    # Add spanning headers for flextable
    # span_cbind is a named list where names are header labels and values are column ranges
    
    # Collect all spans to add in one go (flextable works better this way)
    ncols <- ncol(tab$body$dataset)
    colwidths <- rep(1, ncols)
    values <- rep("", ncols)
    
    # Process spans to create colwidths and values
    for (label in names(span_cbind)) {
      col_range <- span_cbind[[label]]
      start_col <- min(col_range)
      end_col <- max(col_range)
      span_width <- end_col - start_col + 1
      
      # Set colwidth for this span
      colwidths[start_col] <- span_width
      if (span_width > 1) {
        # Mark spanned columns as 0 width (they get merged)
        for (i in (start_col + 1):end_col) {
          if (i <= ncols) colwidths[i] <- 0
        }
      }
      
      # Set the label
      values[start_col] <- label
    }
    
    # Filter out zero-width columns for flextable::add_header_row
    non_zero_indices <- which(colwidths > 0)
    filtered_colwidths <- colwidths[non_zero_indices]
    filtered_values <- values[non_zero_indices]
    
    # Add the header row
    out <- flextable::add_header_row(out, colwidths = filtered_colwidths, values = filtered_values)
    # Center align the spanning headers
    out <- flextable::align(out, i = 1, align = "center", part = "header")
  }
  return(out)
}
