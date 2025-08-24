#' Extract estimates and statistics from a single model
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return data.frame with side-by-side model summaries
#' @noRd
format_estimates <- function(
  est,
  estimate,
  estimate_label,
  statistic,
  vcov,
  conf_level,
  fmt,
  stars,
  shape,
  group_name,
  exponentiate,
  ...
) {
  # when length(estimate) > 1, we want different stat and we want to allow labels
  if (is.null(estimate_label)) estimate_label <- "estimate"

  # conf.int to glue
  estimate_glue <- ifelse(
    estimate == "conf.int",
    "[{conf.low}, {conf.high}]",
    estimate
  )

  statistic_glue <- ifelse(
    statistic == "conf.int",
    "[{conf.low}, {conf.high}]",
    statistic
  )

  # estimate to glue
  estimate_glue <- ifelse(
    grepl("\\{", estimate_glue),
    estimate_glue,
    sprintf("{%s}", estimate_glue)
  )

  # statistics to glue
  if ("statistic" %in% shape$rhs) {
    # don't add parentheses
    statistic_glue <- ifelse(
      grepl("\\{", statistic_glue),
      statistic_glue,
      sprintf("{%s}", statistic_glue)
    )
  } else if (
    !is.null(vcov) && # don't add parentheses
      is.character(vcov) && # to manual strings
      length(vcov) > 1
  ) {
    # of length greater than 1 (i.e., robust shortcuts)
    statistic_glue <- ifelse(
      grepl("\\{", statistic_glue),
      statistic_glue,
      sprintf("{%s}", statistic_glue)
    )
  } else {
    statistic_glue <- ifelse(
      grepl("\\{", statistic_glue),
      statistic_glue,
      sprintf("({%s})", statistic_glue)
    )
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
    if (!'p.value' %in% colnames(est)) {
      stop(
        'To use the `stars` argument, the `get_estimates(model)` function must produce a column called "p.value"',
        call. = FALSE
      )
    }
    # we always want stars, regardless of argument value, in case there is a glue {stars}
    if (is.logical(stars)) {
      stars_tmp <- TRUE
    } else {
      # custom stars user input
      stars_tmp <- stars
    }
    est$stars <- make_stars(est$p.value, stars_tmp)
    est$stars[is.na(est$stars)] <- ""
  }

  # otherwise we get stars in `estimate` even when stars=FALSE and another glue
  # string includes {stars}`
  if (isTRUE(is_star) && isFALSE(is_glue) && !isFALSE(stars)) {
    estimate_glue[1] <- paste0(estimate_glue[1], "{stars}")
  }

  # check if statistics are available -- too complicated for glue strings
  statistic_nonglue <- c(estimate, statistic)
  statistic_nonglue <- statistic_nonglue[!grepl("\\{", statistic_nonglue)]
  statistic_nonglue[statistic_nonglue == "conf.int"] <- "conf.low"
  for (s in statistic_nonglue) {
    if (!s %in% colnames(est)) {
      stop(
        sprintf(
          "`%s` is not available. The `estimate` and `statistic` arguments must correspond to column names in the output of this command: `get_estimates(model)`",
          s
        ),
        call. = FALSE
      )
    }
  }

  # reference categories (while still numeric)
  if (
    "include_reference" %in%
      names(list(...)) &&
      all(c("estimate", "std.error") %in% colnames(est))
  ) {
    idx_ref <- est$estimate == 0 & is.na(est$std.error)
  } else {
    idx_ref <- rep(FALSE, nrow(est))
  }

  # exponentiate
  if (isTRUE(exponentiate)) {
    # standard error before transforming estimate
    cols <- c("std.error", "estimate", "conf.low", "conf.high")
    cols <- intersect(cols, colnames(est))

    # Create a mask for non-random effects
    non_random <- rep(TRUE, nrow(est))
    if (all(c("term", "effect") %in% colnames(est))) {
      non_random <- ifelse(
        grepl("^SD |^Cor ", est$term) & est$effect == "random",
        FALSE,
        TRUE
      )
    }
    if ("component" %in% colnames(est)) {
      non_random <- ifelse(
        est$component == "dispersion",
        FALSE,
        non_random
      )
    }

    for (col in cols) {
      if (col == "std.error") {
        est[["std.error"]][non_random] <- exp(est[["estimate"]][non_random]) *
          est[["std.error"]][non_random]
      } else if (col %in% c("estimate", "conf.low", "conf.high")) {
        est[[col]][non_random] <- exp(est[[col]][non_random])
      }
    }
  }

  ## group names should be raw characters; we don't want to round cyl=4 as 4.000
  for (n in colnames(est)) {
    if (n %in% shape$group_name) {
      est[[n]] <- as.character(est[[n]])
    }
  }

  ## round all
  ## ensures that the reshape doesn't produce incompatible types
  ## exclude factors and characters, otherwise `rounding` will escape them
  ## which is premature since we then call coef_map

  if (inherits(fmt, "fmt_factory")) {
    est <- fmt(est)
  } else {
    stop("must use fmt_factory")
  }

  # needed to avoid empty glues "p =""
  for (i in seq_along(est)) {
    if (is.character(est[[i]])) {
      est[[i]][is.na(est[[i]])] <- ""
    }
  }

  # modelplot safety hack: statistics may not be available for some models (e.g., "brms")
  if (
    identical(
      estimate_glue,
      "{estimate}|{std.error}|{conf.low}|{conf.high}|{p.value}"
    )
  ) {
    if (!"std.error" %in% colnames(est)) {
      estimate_glue <- gsub("{std.error}", " ", estimate_glue, fixed = TRUE)
    }
    if (!"p.value" %in% colnames(est)) {
      estimate_glue <- gsub("{p.value}", " ", estimate_glue, fixed = TRUE)
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

  # reference level: only first category, rest should be empty
  est[["modelsummary_tmp1"]][idx_ref] <- "-"

  if (!is.null(group_name)) {
    miss <- setdiff(shape$group_name, colnames(est))
    if (length(miss) > 0) {
      msg <- sprintf(
        'Group columns (%s) were not found in the extracted data. The "group" argument must be a column name in the data.frame produced by `get_estimates(model)`.  If you wish to combine models with and without grouped estimates, you will find examples on the modelsummary website:',
        paste(miss, collapse = ", ")
      )
      msg <- c(msg, "https://modelsummary.com")
      insight::format_error(msg)
    }
  } else if (is.null(group_name) && !"group" %in% colnames(est)) {
    # cannot be NA because we need to merge
    est[["group"]] <- ""
    group_name <- "group"
  }

  # subset columns
  # "group" is used by parameters() to keep lme4::lmer() types of coefficients together
  cols <- c(
    group_name,
    'group',
    'term',
    paste0('modelsummary_tmp', seq_along(estimate_glue))
  )
  cols <- intersect(cols, colnames(est))
  est <- est[, cols, drop = FALSE]

  # reshape to vertical
  # sort by statistic to have se below coef, but don't sort terms
  # use factor hack to preserve order with weird names like cor__(Inter.)
  # especially important for broom.mixed models
  for (col in c(group_name, "term")) {
    est[[col]] <- as.character(est[[col]]) # otherwise weird rounding
    est[[col]] <- factor(est[[col]], unique(est[[col]]))
  }

  est <- stats::reshape(
    data.frame(est),
    varying = grep("modelsummary_tmp\\d+$", colnames(est), value = TRUE),
    times = grep("modelsummary_tmp\\d+$", colnames(est), value = TRUE),
    v.names = "modelsummary_value",
    timevar = "statistic",
    direction = "long"
  )
  est$id <- NULL

  # sort then convert back to character
  est <- est[
    do.call("order", as.list(est[, c(group_name, "term", "statistic")])),
  ]
  for (col in c(group_name, "term")) {
    est[[col]] <- as.character(est[[col]])
  }

  # statistics need informative names
  idx <- as.numeric(factor(est$statistic))
  # estimates are one per model, but all displayed on the same row, so we give
  # the same identifier. statistics have different names because they need to
  # be merged.
  est$statistic <- c(estimate_label, statistic)[idx]

  # drop empty rows (important for broom.mixed which produces group
  # estimates without standard errors)
  est <- est[!est$modelsummary_value %in% c("", "()", "(NA)"), ]

  # output
  return(est)
}
