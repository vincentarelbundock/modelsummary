#' rename and reorder estimates from a *single* model
#' (before merging to collapse)
#'
#' @keywords internal
map_estimates <- function(
  estimates,
  coef_rename,
  coef_map,
  coef_omit,
  group_map
) {
  # ambiguous rename
  check_dups <- function(dict) {
    dups <- unique(dict[duplicated(dict)])
    for (d in dups) {
      tmp <- names(dict)[which(d == dict)]
      tmp <- intersect(tmp, estimates$term)
      if (length(tmp) > 1) {
        msg <- sprintf(
          "These variables have been assigned the same label in `coef_map` or `coef_rename`, but they are all part of the same model: %s",
          paste(tmp, collapse = ", ")
        )
        stop(msg, call. = FALSE)
      }
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
    check_dups(dict)
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
    check_dups(coef_map)
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
      estimates[[group_name]] %in% names(group_map),
      ,
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

  return(estimates)
}
