get_variable_labels_models <- function(models) {
    out <- list()
    for (mod in models) {
        dat <- hush(insight::get_data(mod))
        lab <- get_variable_labels_data(dat)
        out <- append(out, list(lab))
    }
    out <- lapply(out, function(x) data.frame(clean = names(x), raw = x))
    out <- do.call("rbind", out)
    out <- unique(out)
    if (anyDuplicated(out$clean)) {
        msg <- 
        "Some variables share a name but have inconsistent labels. The labels will be ignored."
        warn(msg, call. = FALSE)
    }
    out <- stats::setNames(out$raw, out$clean)
    return(out)
}


get_variable_labels_data <- function(data) {
    # global variables: sjlabelled-style
    lab <- attr(data, "label", exact = TRUE)
    # variable attributes: haven-style
    if (is.null(lab)) {
        lab <- Filter(
            function(x) inherits(data[[x]], c("labelled", "haven_labelled")),
            colnames(data))
        lab <- sapply(lab, function(x) attr(data[[x]], "label"))
    }
    if (length(lab) == 0) {
        lab <- NULL
    }
    return(lab)
}
