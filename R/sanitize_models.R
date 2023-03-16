sanitize_models <- function(models, ...) {

    # before sanity_vcov
    # models must be a list use first class rather than `inherits` because some
    # models inherit from list
    if (class(models)[1] != "list") {
        models <- list(models)
    }

    # fixest::fixest_multi
    # handles cases with `split` or with both `fsplit` and ` do this before
    # wrapping into a list vincent personally uses this type of model a lot,
    # but he does not want to hard-code a ton of exceptions like this.
    fixest_multi_to_list <- function(i) {
        if (!inherits(models[[i]], "fixest_multi")) {
            # list() because c() later
            return(stats::setNames(list(models[[i]]), names(models)[i]))
        }
        # no names or default names
        if (is.null(names(models[[i]])) || all(grepl("^lhs|^sample.var", names(models[[i]])))) {
            nam <- paste(names(models)[i], fixest_multi_names(models[[i]]))
        } else {
            nam <- paste(names(models)[i], names(models[[i]]))
        }
        stats::setNames(as.list(models[[i]]), nam)
    }

    out <- list()
    for (i in seq_along(models)) {
        out <- c(out, fixest_multi_to_list(i))
    }

    for (m in out) {
        sanity_dots(m, ...)
    }

    return(out)
}
