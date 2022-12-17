sanitize_models <- function(models, ...) {

    # fixest::fixest_multi
    # handles cases with `split` or with both `fsplit` and ` do this before
    # wrapping into a list vincent personally uses this type of model a lot,
    # but he does not want to hard-code a ton of exceptions like this.

    if (inherits(x = models, "fixest_multi")) {
        # no names or default names

        if (is.null(names(models)) || all(grepl("^lhs|^sample.var", names(models)))) {
            nam <- fixest_multi_names(models)
        } else {
            nam <- names(models)
        }
        models <- stats::setNames(as.list(models), nam)
    }

    # before sanity_vcov
    # models must be a list use first class rather than `inherits` because some
    # models inherit from list
    if (class(models)[1] != "list") {
        models <- list(models)
    }

    for (m in models) {
        sanity_dots(m, ...)
    }

    return(models)
}
