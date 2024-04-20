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
        warning(msg, call. = FALSE)
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
        lab <- sapply(lab, function(x) attr(data[[x]], "label", exact = TRUE))
    }
    if (length(lab) == 0) {
        lab <- NULL
    }
    for (n in names(lab)) {
        if (is.null(lab[[n]])) {
            lab[[n]] <- names(n)
        }
    }
    return(lab)
}


coef_rename_labels <- function(x, dict) {
    out <- x
    for (i in seq_along(dict)) {
        # escape because user-supplied labels could include special regex characters like parentheses.
        # substrings are dangerous because they could be subbed twice. We
        # probably can't format `cyl4`, `cyl6` because those could be
        # user-supplied variable names.
        # pad otherwise we get ugly labels like "Cylinders6"
        tar <- dict[i]
        src <- names(dict)[i]
        src <- gsub("(\\W)", "\\\\\\1", src) # escape parentheses so they don't catch in regex
        out <- gsub(sprintf("^%s$", src), tar, out, perl = TRUE)
        out <- gsub(sprintf("^%s:", src), paste0(tar, ":"), out, perl = TRUE)
        out <- gsub(sprintf(":%s$", src), paste0(":", tar), out, perl = TRUE)
        out <- gsub(sprintf(":%s:", src), paste0(":", tar, ":"), out, perl = TRUE)
        out <- gsub(sprintf("factor\\(%s\\)", src), paste0(tar, " "), out, perl = TRUE)
    }
    out <- trimws(out)
    return(out)
}


strip_labels <- function(data) {
    for (x in colnames(data)) {
        class(data[[x]]) <- setdiff(class(data[[x]]), c("haven_labelled", "vctrs_vctr"))
        attr(data[[x]], "label") <- NULL
    }
    return(data)
}
