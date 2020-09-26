#' Extract estimates and statistics from a single model
#' @importFrom generics tidy
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return data.frame with side-by-side model summaries
#' @keywords internal
extract_estimates <- function(model,
                              statistic = 'std.error',
                              statistic_override = NULL,
                              statistic_vertical = TRUE,
                              conf_level = .95,
                              fmt = '%.3f',
                              stars = FALSE,
                              estimate = 'estimate',
                              ...) {



  # statistic override
  if (!is.null(statistic_override)) {

    # extract overriden statistics
    so <- extract_statistic_override(model,
      statistic_override=statistic_override,
      conf_level=conf_level)

    bad1 <- (statistic != "conf.int") & (!statistic %in% colnames(so))
    bad2 <- (statistic == "conf.int") & (!"conf.low" %in% colnames(so))
    if (bad1 || bad2) {
      stop(paste0(statistic, " cannot be extracted through the
                        `statistic_override` argument. You might want to look
                        at the `modelsummary:::extract_statistic_override`
                        function to diagnose the problem."))
    } 

    # extract estimates
    est <- tidy(model, ...)

    # keep only columns that do not appear in so
    est <- est[, c('term', base::setdiff(colnames(est), colnames(so))), drop = FALSE]
    est <- dplyr::left_join(est, so, by = 'term')

  } else { # if statistic_override is not used

    # extract estimates
    if ('conf.int' %in% statistic) {
      est <- tidy(model, conf.int = TRUE, conf.level = conf_level, ...)
    } else {
      est <- tidy(model, ...)
    }
  }

  # tidy_custom if availablee
  est_custom <- tidy_custom(model)

  # did tidy and tidy_custom produce useable output?
  sanity_tidy(est, est_custom, estimate, statistic, class(model)[1])

  # combine (or overwrite) tidy and tidy_custom
  if (!is.null(est_custom)) {
    for (n in colnames(est_custom)) {
      est[[n]] <- est_custom[[n]]
    }
  }

  # broom.mixed sometimes includes a `group` column with duplicate terms
  if ("group" %in% colnames(est) && anyDuplicated(est$term) > 0) {
    est$term <- ifelse(is.na(est$group), est$term, paste(est$group, est$term))
  }

  # round estimates
  est[[estimate]] <- rounding(est[[estimate]], fmt)

  # stars
  if (!isFALSE(stars)) {
    if (!'p.value' %in% colnames(est)) {
      stop('To use the `stars` argument, the `tidy` function must produce a column called "p.value"')
    }
    est[[estimate]] <- make_stars(est[[estimate]], 
                                  est$p.value,
                                  stars)
  }


  # extract statistics
  for (i in seq_along(statistic)) {

    s <- statistic[i]

    # extract confidence intervals
    if (s == 'conf.int') {
      if (!all(c('conf.low', 'conf.high') %in% colnames(est))) {
        stop('tidy cannot not extract confidence intervals for a model of this class.')
      }

      # rounding and brackets
      est$conf.high <- rounding(est$conf.high, fmt)
      est$conf.low <- rounding(est$conf.low, fmt)
      est[[paste0('statistic', i)]] <- paste0('[', est$conf.low, ', ', est$conf.high, ']')

      # extract other types of statistics
    } else {

      # rounding non-character values and parentheses
      if (!is.character(est[[s]])) {
        est[[s]] <- rounding(est[[s]], fmt)
        est[[s]] <- ifelse(est[[s]] != '', # avoid empty parentheses for NAs
          paste0('(', est[[s]], ')'),
          est[[s]])
        est[[paste0('statistic', i)]] <- est[[s]]
      } else if (is.character(est[[s]])) {
        # renaming character columns for later subsetting
        est[[paste0('statistic', i)]] <- est[[s]]
      }

    }
  }

  # subset columns
  cols <- c('term', estimate, paste0('statistic', seq_along(statistic)))
  est <- est[, cols, drop = FALSE]

  # reshape to vertical
  if (statistic_vertical) {
    est <- est %>%
      tidyr::pivot_longer(-term, names_to = 'statistic') %>%
      # sort by statistic to have se below coef, but don't sort terms
      # use factor hack to preserve order with weird names like cor__(Inter.)
      # especially important for broom.mixed style models 
      dplyr::mutate(term = factor(term, unique(term))) %>%
      dplyr::arrange(term, statistic) %>%
      dplyr::mutate(term = as.character(term))
  } else {
    est$statistic <- 'estimate'
    est$value <- paste(est[[estimate]], est$statistic1)
    est[[estimate]] <- est$statistic1 <- NULL
  }

  # drop empty rows (important for broom.mixed which produces group
  # estimates without standard errors)
  est <- est[est$value != "", ]

  # output
  return(est)
}
