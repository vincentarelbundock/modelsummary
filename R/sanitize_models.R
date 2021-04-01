sanitize_models <- function(models) {

  # fixest::fixest_multi
  # handles cases with `split` or with both `fsplit` and `
  # do this before wrapping into a list
  # vincent personally uses this type of model a lot, but he might not want to
  # hard-code a ton of exceptions like this.
  fixest_multi_names <- function(x) {
    att <- attributes(x)$meta
    out <- rep("", nrow(att$tree))
    if ("lhs" %in% names(att$tree)) {
      out <- paste0(out, att$all_names$lhs[att$tree$lhs], " ")
    }
    if ("sample" %in% names(att$tree)) {
      out <- paste0(out, att$all_names$sample[att$tree$sample])
    }
    if (all(out == "")) {
      out <- NULL
    }
    return(out)
  }

  if (class(models)[1] == "fixest_multi") {
    if (is.null(names(models)) ||
        length(names(models)) != nrow(attributes(models)$meta$tree)) {
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
