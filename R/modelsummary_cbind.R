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
    } else {
        models <- do.call(c, models)
    }

    out <- list(
        models = models,
        shape = NULL,
        span_cbind = spans)

    return(out)
}


set_span_cbind <- function(tab, span_cbind) {
    out <- tab
    if (!is.null(span_cbind) && inherits(tab, "tinytable")) {
        out <- tinytable::group_tt(out, j = span_cbind)
    }
    return(out)
}