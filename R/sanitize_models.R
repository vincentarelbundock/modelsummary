sanitize_models <- function(models) {

    # fixest::fixest_multi
    # handles cases with `split` or with both `fsplit` and `
    # do this before wrapping into a list
    # vincent personally uses this type of model a lot, but he might not want to
    # hard-code a ton of exceptions like this.
    fixest_multi_names <- function(x) {
        tree <- fixest::models(x)
        if ("lhs" %in% names(tree)) {
            out <- tree$lhs
        } else if ("sample" %in% names(tree)) {
            out <- sprintf("%s: %s", tree$sample.var, tree$sample)
        } else {
            out <- paste("Model", seq_along(x))
        }
        return(out)
    }
    if (inherits(models, "fixest_multi")) {
        insight::check_if_installed("fixest", minimum_version = "0.10.5")
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

    return(models)
}
