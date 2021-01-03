#' Extract goodness-of-fit statistics from a single model
#' @param model object type with an available `glance` method.
#' @inheritParams modelsummary
#' @return data.frame with goodness-of-fit  statistics
#' @noRd
extract_gof <- function(model, fmt, gof_map, ...) {

  # extract gof from model object
  gof <- get_gof(model, ...)
 
  # lm model: include F-stat by default
  # glm also inherits from lm
  if (isTRUE(class(model)[1] == "lm") && 
      "statistic" %in% colnames(gof)) {
    gof$F <- gof$statistic
  }

  # glance_custom
  gof_custom <- glance_custom(model)
  sanity_gof(gof, gof_custom)
  if (!is.null(gof_custom)) {
    for (n in colnames(gof_custom)) {
      gof[[n]] <- gof_custom[[n]]
    }
  }

  # convert gof_map to list and vectors
  if (is.null(gof_map)) {
    gm_list <- lapply(1:nrow(modelsummary::gof_map), function(i) 
                  as.list(modelsummary::gof_map[i, ]))
    gm_omit <- sapply(gm_list, function(x) x$omit) 
  } else if (inherits(gof_map, "data.frame")) {
    gm_list <- lapply(1:nrow(gof_map), function(i)
                  as.list(gof_map[i, ]))
  } else {
    gm_list <- gof_map
  }
  gm_raw <- sapply(gm_list, function(x) x$raw)
  gm_clean <- sapply(gm_list, function(x) x$clean) 

  # round 
  unknown <- setdiff(colnames(gof), gm_raw)
  for (u in unknown) {
    gof[[u]] <- rounding(gof[[u]], fmt)
  }
  for (gm in gm_list) {
    if (gm$raw %in% colnames(gof)) {
      gof[[gm$raw]] <- rounding(gof[[gm$raw]], gm$fmt)
    }
  }

  # gof_map = NULL: drop explicit omit
  if (is.null(gof_map)) {
    gof <- gof[, !colnames(gof) %in% gm_raw[gm_omit], drop=FALSE]
  # gof_map != NULL: drop unknown
  } else {
    gof <- gof[, colnames(gof) %in% gm_raw, drop=FALSE]
  }

  # reorder
  idx1 <- intersect(gm_raw, colnames(gof))
  idx2 <- setdiff(colnames(gof), gm_raw)
  gof <- gof[c(idx1, idx2)]

  # some gof were kept
  if (ncol(gof) > 0) {
    # rename
    idx <- match(colnames(gof), gm_raw)
    colnames(gof) <- ifelse(is.na(idx), colnames(gof), gm_clean[idx])

    # reshape
    out <- data.frame(term = names(gof), value = unlist(gof))
  
  # all gof are excluded return an empty tibble (needs character to match merge type)
  } else { 
    out <- data.frame(term = NA_character_, value = NA_character_)
    out <- stats::na.omit(out)
  }

  # factor to character (otherwise gof_map breaks under R < 4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }

  # output
  return(out)
}


#' Extract model gof A mostly internal function with some potential uses
#' outside.
#'
#' @inheritParams get_estimates
#' @export
get_gof <- function(model, ...) {

  flag <- function(x) {
    inherits(x, "data.frame") &&
    nrow(x) == 1
  }

  # generics first
  gof <- suppressWarnings(try(
    generics::glance(model, ...), silent=TRUE))
  if (flag(gof)) return(gof)

  # broom second
  gof <- suppressWarnings(try(
    broom::glance(model, ...), silent=TRUE))
  if (flag(gof)) return(gof)

  # performance third
  f <- function(model, ...) {
    error_msg <- utils::capture.output(out <- performance::model_performance(model))
    out <- insight::standardize_names(out, style="broom")
    mi <- insight::model_info(model)
    # nobs
    if ("n_obs" %in% names(mi)) {
      out$nobs <- mi$n_obs
    }
    return(out)
  }
  gof <- suppressWarnings(try(
    f(model, ...), silent=TRUE))
  if (flag(gof)) return(gof)

  # broom.mixed fourth
  if (check_dependency("broom.mixed")) {
    gof <- suppressWarnings(try(
      broom.mixed::glance(model, ...), silent=TRUE))
    if (flag(gof)) return(gof)
  }

  stop(sprintf('Cannot extract information from models of class "%s". Consider installing `broom.mixed` or any other package with `tidy` and `glance` functions appropriate for this model type. Alternatively, you can define your own `tidy` method, following the instructions on the `modelsummary` website: https://vincentarelbundock.github.io/modelsummary/articles/newmodels.html', class(model)[1]))

}
