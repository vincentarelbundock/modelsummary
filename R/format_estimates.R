#' Extract estimates and statistics from a single model
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return data.frame with side-by-side model summaries
#' @noRd
format_estimates <- function(
  est,
  estimate,
  statistic,
  vcov,
  conf_level,
  fmt,
  stars,
  shape,
  group_name,
  exponentiate,
  ...) {

  # conf.int to glue
  estimate_glue <- ifelse(
    estimate == "conf.int",
    "[{conf.low}, {conf.high}]",
    estimate)

  statistic_glue <- ifelse(
    statistic == "conf.int",
    "[{conf.low}, {conf.high}]",
    statistic)

  # estimate to glue
  estimate_glue <- ifelse(
    grepl("\\{", estimate_glue),
    estimate_glue,
    sprintf("{%s}", estimate_glue))

  # statistics to glue
  if ("statistic" %in% shape$rhs) { # don't add parentheses
     statistic_glue <- ifelse(
      grepl("\\{", statistic_glue),
      statistic_glue,
      sprintf("{%s}", statistic_glue))
  } else if (!is.null(vcov) &&     # don't add parentheses
      is.character(vcov) && # to manual strings
      length(vcov) > 1) {   # of length greater than 1 (i.e., robust shortcuts)
    statistic_glue <- ifelse(
      grepl("\\{", statistic_glue),
      statistic_glue,
      sprintf("{%s}", statistic_glue))
  } else {
    statistic_glue <- ifelse(
      grepl("\\{", statistic_glue),
      statistic_glue,
      sprintf("({%s})", statistic_glue))
  }

  # combine estimate and statistics
  estimate_glue <- c(estimate_glue, statistic_glue)

  # stars (before rounding)
  # compute stars if:
  #   a) stars is not FALSE
  #   b) {stars} is in glue string
  # paste stars if:
  #   a) stars is not FALSE and is not a glue string
  is_star <- !isFALSE(stars) || isTRUE(any(grepl("\\{stars\\}", estimate_glue)))
  is_glue <- grepl("\\{", estimate)
  if (is_star) {
    # glue string can include stars even if stars=FALSE
    if (isFALSE(stars)) {
      stars <- TRUE
    }
    if (!'p.value' %in% colnames(est)) {
      stop('To use the `stars` argument, the `tidy` function must produce a column called "p.value"',
           call. = FALSE)
    }
    est$stars <- make_stars(est$p.value, stars)
    est$stars[is.na(est$stars)] <- ""
  }
  if (isTRUE(is_star) && isFALSE(is_glue)) {
      estimate_glue[1] <- paste0(estimate_glue[1], "{stars}")
  }

  # check if statistics are available -- too complicated for glue strings
  statistic_nonglue <- c(estimate, statistic)
  statistic_nonglue <- statistic_nonglue[!grepl("\\{", statistic_nonglue)]
  statistic_nonglue[statistic_nonglue == "conf.int"] <- "conf.low"
  for (s in statistic_nonglue) {
    if (!s %in% colnames(est)) {
      stop(sprintf("`%s` is not available. The `estimate` and `statistic` arguments must correspond to column names in the output of this command: `get_estimates(model)`", s), call. = FALSE)
    }
  }

  # exponentiate
  if (isTRUE(exponentiate)) {

    # standard error before transforming estimate
    cols <- c("std.error", "estimate", "conf.low", "conf.high")
    cols <- intersect(cols, colnames(est))
    for (col in cols) {
      if (col == "std.error") {
        est[["std.error"]] <- exp(est[["estimate"]]) * est[["std.error"]]
      } else if (col %in% c("estimate", "conf.low", "conf.high")) {
        est[[col]] <- exp(est[[col]])
      }
    }
  }


  ## round all
  ## ensures that the reshape doesn't produce incompatible types
  ## exclude factors and characters, otherwise `rounding` will escape them
  ## which is premature since we then call coef_map
  for (n in colnames(est)) {
    if (!is.character(est[[n]]) && !is.factor(est[[n]])) {
      if (n %in% names(fmt)) {
        fmt1 <- fmt[[n]]
      } else {
        fmt1 <- fmt[["fmt"]]
      }
      # keep as numeric foir glue functions
      if (!is.null(fmt1)) {
        est[[n]] <- rounding(est[[n]], fmt1)
      }
    }
  }

  # extract estimates (there can be several)
  for (i in seq_along(estimate_glue)) {
    s <- estimate_glue[i]
    # At this point, NAs are "". `glue_data(.na=NULL)` works with NAs
    tmp <- est
    tmp <- glue::glue_data(tmp, s, .na = " ")

    # remove common patterns of missing data, but we can't catch everything
    # since strings are user-arbitrary...
    tmp <- gsub("\\s*\\[[\\s|,]*\\]", "", tmp, perl = TRUE) # [, ] or ()
    tmp <- gsub("\\s*\\([\\s|,]*\\)", "", tmp, perl = TRUE) # (, ) or ()
    tmp[tmp == gsub("\\{.*\\}", "", s)] <- "" # glue without {} means we failed
    tmp <- trimws(tmp)

    tmp[is.na(tmp)] <- ""
    est[[paste0("modelsummary_tmp", i)]] <- as.character(tmp)
    # avoid empty parentheses for NAs
    est[[paste0("modelsummary_tmp", i)]][est[[s]] == ""] <- ""
  }


  if (!is.null(group_name) && group_name %in% colnames(est)) {
    est[["group"]] <- est[[group_name]]

  } else if (!is.null(group_name)) {
    est[["group"]] <- ""
    warning(sprintf('Group name "%s" was not found in the extracted data. The "group" argument must be a column name in the data.frame produced by `get_estimates(model)`.  If you wish to combine models with and without grouped estimates, you will find examples on the modelsummary website:
https://vincentarelbundock.github.io/modelsummary', group_name),
            call. = FALSE)

  } else if (!"group" %in% colnames(est)) {
    # cannot be NA because we need to merge
    est[["group"]] <- ""
  }

  # subset columns
  cols <- c('group', 'term', paste0('modelsummary_tmp', seq_along(estimate_glue)))
  cols <- intersect(cols, colnames(est))
  est <- est[, cols, drop = FALSE]

  # reshape to vertical
  # sort by statistic to have se below coef, but don't sort terms
  # use factor hack to preserve order with weird names like cor__(Inter.)
  # especially important for broom.mixed models
  est$term <- factor(est$term, unique(est$term))
  est <- stats::reshape(
    data.frame(est),
    varying       = grep("modelsummary_tmp\\d+$", colnames(est), value = TRUE),
    times         = grep("modelsummary_tmp\\d+$", colnames(est), value = TRUE),
    v.names       = "value",
    timevar       = "statistic",
    direction     = "long")
  est$id <- NULL

  # sort then convert back to character
  est <- est[order(est$term, est$statistic), ]
  est$term <- as.character(est$term)

  # statistics need informative names
  idx <- as.numeric(factor(est$statistic))
  # estimates are one per model, but all displayed on the same row, so we give
  # the same identifier. statistics have different names because they need to
  # be merged.
  est$statistic <- c("estimate", statistic)[idx]

  # drop empty rows (important for broom.mixed which produces group
  # estimates without standard errors)
  est <- est[!est$value %in% c("", "()", "(NA)"), ]

  # output
  return(est)
}
