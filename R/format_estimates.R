#' Extract estimates and statistics from a single model
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return data.frame with side-by-side model summaries
#' @noRd
format_estimates <- function(
  est,
  estimate   = "estimate",
  statistic  = "std.error",
  vcov       = NULL,
  conf_level = .95,
  fmt        = "%.3f",
  stars      = FALSE,
  group_name = NULL,
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
  if (!is.null(vcov) &&     # don't add parentheses
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
      stop('To use the `stars` argument, the `tidy` function must produce a column called "p.value"')
    }
    est$stars <- make_stars(est$p.value, stars)
    est$stars[is.na(est$stars)] <- ""
  }
  if (isTRUE(is_star) && isFALSE(is_glue)) {
      estimate_glue[1] <- paste0(estimate_glue[1], "{stars}")
  }


  ## I would like to check if the statistics are available and return an
  ## informative message, but this does not seem possible for complicated glue
  ## strings with functions:
  ## 'p{ifelse(p.value <0.001, "Significant", "Not significant")'

  ## # are statistics available? if not, display an informative error message
  ## # check this after all custom statistics and stars are added
  ## estimate_glue_strip <- regmatches(estimate_glue, gregexpr("\\{[^\\}]*\\}", estimate_glue))
  ## estimate_glue_strip <- sort(unique(unlist(estimate_glue_strip)))
  ## estimate_glue_strip <- gsub("\\{|\\}", "", estimate_glue_strip)
  ## estimate_glue_strip <- setdiff(estimate_glue_strip, colnames(est))
  ## if (length(estimate_glue_strip) > 0) {
  ##   stop(sprintf("These estimates or statistics do not seem to be available: %s. You can use the `get_estimates` function to see which statistics are available.",
  ##                paste(estimate_glue_strip, collapse = ", ")))
  ## }


  ## round all 
  ## ensures that the reshape doesn't produce incompatible types
  ## exclude factors and characters, otherwise `rounding` will escape them
  ## which is premature since we then call coef_map
  for (n in colnames(est)) {
    if (!is.character(est[[n]]) && !is.factor(est[[n]])) {
      est[[n]] <- rounding(est[[n]], fmt)
    }
  }


  # extract estimates (there can be several)
  for (i in seq_along(estimate_glue)) {
    s <- estimate_glue[i]
    est[[paste0("modelsummary_tmp", i)]] <- as.character(glue::glue_data(est, s))
    # avoid empty parentheses for NAs
    est[[paste0("modelsummary_tmp", i)]][est[[s]] == ""] <- ""
  }


  if (!is.null(group_name) && group_name %in% colnames(est)) {
    est[["group"]] <- est[[group_name]]
  } else if (!is.null(group_name)) {
    est[["group"]] <- ""
    warning(sprintf('Group name "%s" was not found in the extracted data. The "group" argument must be a column name in the data.frame produced by `get_estimates(model)`.  If you wish to combine models with and without grouped estimates, you will find examples on the modelsummary website:
https://vincentarelbundock.github.io/modelsummary', group_name))
  } else {
    # cannot be NA because we need to merge
    est[["group"]] <- ""
  }


  # subset columns
  cols <- c('group', 'term',
            paste0('modelsummary_tmp', seq_along(estimate_glue)))
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

  # drop empty rows (important for broom.mixed which produces group
  # estimates without standard errors)
  est <- est[!est$value %in% c("", "()", "(NA)"), ]

  # output
  return(est)
}
