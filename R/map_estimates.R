#' rename and reorder estimates from a *single* model
#' (before merging to collapse)
#'
#' @keywords internal
map_estimates <- function(
    estimates,
    coef_rename,
    coef_map,
    coef_omit,
    group_map,
    shape = NULL) {
  # ambiguous rename
  check_dups <- function(estimates, shape) {
    cols <- grep(
      "term|group|component|statistic|^contrast|^part",
      colnames(estimates),
      value = TRUE)
    # unknown shape variables (ex: marginaleffects `by="am"`)
    if (isTRUE(checkmate::check_formula(shape[["shape_formula"]]))) {
      cols <- c(cols, all.vars(shape[["shape_formula"]]))
    }
    cols <- intersect(cols, colnames(estimates))
    dups <- as.data.frame(estimates)[, cols, drop = FALSE]
    if (nrow(estimates) != nrow(unique(dups))) {
      msg <- "There are duplicate labels in the estimates table. Call `get_estimates()` to see the available identifiers, and use the `shape` argument to specify the (potentially nested or grouped)  nature of parameters. Alternatively, duplication can happen when you use `coef_map` or `coef_rename` to assign the same label to multiple variables, but these variables are part of the same model. Please check your `coef_map` and `coef_rename` arguments."
      stop(msg, call. = FALSE)
    }
  }

  # coef_omit
  if (is.character(coef_omit)) {
    idx <- !grepl(coef_omit, estimates$term, perl = TRUE)
    if (sum(idx) > 0) {
      estimates <- estimates[idx, , drop = FALSE]
    } else {
      msg <- 'The regular expression supplied to `coef_omit` matched and omitted all the coefficients of (at least) one model, but `modelsummary` requires at least one coefficient to produce a table. Here are some examples of valid regular expressions for different use-cases:

library(modelsummary)
data(trees)
mod <- lm(Girth ~ Height + Volume, data = trees)

# coef_omit: omit coefficients matching one substring
modelsummary(mod, coef_omit = "ei")

# coef_omit: omit a specific coefficient
modelsummary(mod, coef_omit = "^Volume$")

# coef_omit: omit coefficients matching either one of two substring
modelsummary(mod, coef_omit = "ei|rc")

# coef_omit: keep coefficients starting with a substring (using a negative lookahead)
modelsummary(mod, coef_omit = "^(?!Vol)")

# coef_omit: keep coefficients matching either one of two substring
modelsummary(mod, coef_omit = "^(?!.*ei|.*pt)")
'
      stop(msg, call. = FALSE)
    }
  }

  # coef_rename
  # TRUE: done in `get_estimates()` straight from the parameters() attribute
  # FALSE: do nothing
  if (
    !is.null(coef_rename) &&
      !is.logical(coef_rename) &&
      !isTRUE(checkmate::check_character(coef_rename, names = "unnamed"))
  ) {
    # unnamed vectors are processed in `modelsummary` directly because we rely on the order in the final table
    if (isTRUE(checkmate::check_character(coef_rename, names = "unique"))) {
      dict <- coef_rename
    } else if (is.function(coef_rename)) {
      dict <- stats::setNames(coef_rename(estimates$term), estimates$term)
    }
    # if coef_rename is a function, then we trust the user to do interaction
    # replacement. Otherwise, we can get duplicate replacements: "Height (in feet) (in feet)".
    estimates$term <- replace_dict(
      estimates$term,
      dict,
      interaction = !isTRUE(is.function(coef_rename))
    )
  }

  # coef_map
  if (!is.null(coef_map)) {
    if (is.null(names(coef_map))) {
      coef_map <- stats::setNames(coef_map, coef_map)
    }
    idx <- estimates$term %in% names(coef_map)
    if (!any(idx)) {
      stop(
        "At least one of the term names in each model must appear in `coef_map`.",
        call. = FALSE
      )
    }
    estimates <- estimates[idx, , drop = FALSE]
    args <- list(
      match(estimates$term, names(coef_map)),
      seq_len(nrow(estimates))
    )
    estimates$term <- replace_dict(
      estimates$term,
      coef_map,
      interaction = !isTRUE(is.function(coef_rename))
    )
    estimates <- estimates[do.call(order, args), ]
  }

  # group_map
  group_name <- setdiff(colnames(estimates), c("part", "term"))[1] # HACK!
  if (!is.null(group_map)) {
    if (is.null(names(group_map))) {
      group_map <- stats::setNames(group_map, group_map)
    }
    # subset
    estimates <- estimates[
      estimates[[group_name]] %in% names(group_map), ,
      drop = FALSE
    ]
    # reorder
    tmp <- split(estimates, estimates[[group_name]])
    tmp <- tmp[match(names(group_map), names(tmp))]
    estimates <- data.table::rbindlist(tmp)
    # rename
    estimates[[group_name]] <- replace_dict(
      estimates[[group_name]],
      group_map
    )
  }

  check_dups(estimates, shape)

  return(estimates)
}
